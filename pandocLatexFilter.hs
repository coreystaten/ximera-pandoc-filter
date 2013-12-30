
{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (catch)

import Control.Monad
import Text.Pandoc
import Text.Pandoc.Walk
import System.Exit (ExitCode(..))
import System.IO
import System.Environment
import System.Directory (getTemporaryDirectory, removeFile)
import System.FilePath.Posix (dropFileName)
import System.Process (runCommand, waitForProcess)
import Data.Aeson
import Data.Hashable
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Database.MongoDB hiding (lookup, replace, runCommand)

-- Replace these with environments so they're parsed correctly to divs?
commands :: [T.Text]
commands = ["headline", "activitytitle"]

environments :: [T.Text]
environments = ["question", "exercise", "exploration", "answer", "solution"]

-- | The template to use for tikzpictures from the filter, loaded from tikz-template.tex
tikzTemplate :: IO Template
tikzTemplate =
    do
        templateContents <- readFile "tikz-template.tex"
        let result =  case (compileTemplate $ T.pack templateContents) of
                          Left _ -> error "Unable to parse tikz-template.tex"
                          Right template -> template
        return result

-- | Compile the given file as a standalone tikzpicture, to a PNG.
-- Throw an exception on error.
compileTikzFile :: FilePath -- ^ The filename to compile.
                -> FilePath -- ^ The destination filename.
                -> IO ()
compileTikzFile toCompile target =
    do
        -- TODO: mupdf instead of convert?
        -- TODO: Don't forget about security of running other people's pdflatex.  Either sandboxed version, or run this filter with low permissions.
        --       Security plan:  Run the pandoc/filter process using a Linux container or other sandbox; do not acces MongoDB from the filter, but instead move that to Node
        --                       service (since sandbox should disable network access); disable first line configuration of LaTeX files (so user can not enable \write18);
        --                       make sure repos are extracted to distinct directories so that sandboxing does not share repo content.
        pHandle <- runCommand $ T.unpack (T.concat ["pdflatex -output-directory=", T.pack $ dropFileName toCompile, " ", T.pack toCompile,  " > /dev/null && convert -density 600x600 ", toCompilePdf, " -quality 90 -resize 800x600 ", T.pack target, " > /dev/null"])
        exitCode <- waitForProcess pHandle
        removeFile $ T.unpack toCompilePdf
        if exitCode == ExitSuccess then
            return ()
        else
            error "Failure to compile tikzpicture to PNG."
    where toCompilePdf = T.replace ".tex" ".pdf" (T.pack toCompile)


-- | Given content of tikzpicture environment, compile to PNG and return the contents.
tikzpictureToPng :: T.Text      -- ^ The contents of the tikzpicture environment.
                 -> IO B.ByteString -- ^ The content of the resulting PNG file.
tikzpictureToPng content =
    do
        -- Render through Pandoc template.
        template <- tikzTemplate
        let renderedTex = renderTemplate template $ object ["picture" .= content]
        -- Write LaTeX to temporary file.
        tempDirectory <- getTemporaryDirectory
        (fileName, handle) <- openTempFile tempDirectory "tikz.tex"
        hPutStr handle renderedTex
        hClose handle

        -- Compile PNG
        let pngFileName = T.unpack $ T.replace ".tex" ".png" (T.pack fileName)
        compileTikzFile fileName pngFileName

        pngContent <- B.readFile pngFileName

        --  Remove temporary files.
        let logFileName = T.unpack $ T.replace ".tex" ".log" (T.pack fileName)
            auxFileName = T.unpack $ T.replace ".tex" ".aux" (T.pack fileName)
        removeFile fileName
        removeFile logFileName
        removeFile auxFileName
        removeFile pngFileName

        return pngContent

tikzFilter :: T.Text -> Block -> IO Block
tikzFilter repoId b@(RawBlock (Format "latex") s) =
    let
        sT = T.pack s
    in 
        do
            if T.isPrefixOf ("\\begin{tikzpicture}") sT
            then
                do
                    pngContent <- tikzpictureToPng sT

                    -- Hash PNG Contents, and use as id for image in new block returned.
                    let h = abs $ hash pngContent

                    -- Add file contents to MongoDB
                    addPngFileToMongo pngContent h repoId
                    
                    return $ Plain [Image [] ("/tikzpictures/" ++ (show h), "Tikz Picture")]
            else
                return b
tikzFilter _ b = return b

addPngFileToMongo :: B.ByteString -> Int -> T.Text -> IO ()
addPngFileToMongo content h repoId =
    do
        mongoHost <- getEnv "XIMERA_MONGO_URL"
        mongoDatabase <- getEnv "XIMERA_MONGO_DATABASE"
        pipe <- runIOE $ connect (host mongoHost)
        err <- access pipe master (T.pack mongoDatabase) run
        case err of
            Left _ -> error (show err)
            Right _ -> close pipe
    where
        run = do
            insert_ "TikzPngFilesToLoad" ["content" =: Binary content, "hash" =: h, "repoId" =: repoId]

-- | Turn latex RawBlocks for the given environment into Divs with that environment as their class.
-- Normally, these blocks are ignored by HTML writer. -}
environmentFilter :: T.Text -> Map.Map String MetaValue -> Block -> IO Block
environmentFilter e meta b@(RawBlock (Format "latex") s) =
	let
        sLen = length s
    	eLen = T.length e
    in
		if T.isPrefixOf (T.concat ["\\begin{", e, "}"]) (T.pack s)
		then
            let
                content = drop (eLen + 8) $ take (sLen - (eLen + 6)) s
            in
                do
                    blocks <- parseRawBlock content meta
                    return $ Div ("",[T.unpack e],[]) blocks
		else return b
environmentFilter _ _ b = return b

parseRawBlock :: String -> Map.Map String MetaValue -> IO [Block]
parseRawBlock content meta =
    let
        (Pandoc _ blocks) = readLaTeX (def {readerParseRaw = True}) content
    in
        sequence $ map (substituteRawBlocks meta) blocks

environmentFilters :: [Map.Map String MetaValue -> Block -> IO Block]
environmentFilters = map environmentFilter (environments ++ commands)

substituteRawBlocks :: (Map.Map String MetaValue) -> Block -> IO Block
substituteRawBlocks m x =
    do
        let repoId = findRepoId m
        y <- foldM (flip ($)) x (map ($ m) environmentFilters)
        tikzFilter repoId y

findRepoId :: Map.Map String MetaValue -> T.Text
findRepoId m =
    case Map.lookup "repoId" m of
        Just v -> case v of
                      MetaString s -> T.pack s
                      _ -> error "No repo ID filter"
        Nothing -> error "No repo ID passed to filter"
                    

main :: IO ()
main = do
    toJSONFilterMeta substituteRawBlocks

-- Modified version of toJSONFilter, also passing metadata.
toJSONFilterMeta :: (Map.Map String MetaValue -> Block -> IO Block) -> IO ()
toJSONFilterMeta f =
    do
        jsonContents <- BL.getContents
        let doc = either error id . eitherDecode' $ jsonContents
        let meta = case doc of
                       Pandoc m _ -> unMeta m
        processedDoc <- (walkM (f meta) :: Pandoc -> IO Pandoc) $ doc
        BL.putStr . encode $ processedDoc
