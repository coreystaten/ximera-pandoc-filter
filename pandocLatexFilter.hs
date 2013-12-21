
{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (catch)

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
import qualified Data.ByteString.Lazy as B
import Database.MongoDB hiding (lookup, replace, runCommand)

environments :: [T.Text]
environments = ["question", "exercise", "exploration"]

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
        pHandle <- runCommand $ T.unpack (T.concat ["pdflatex -output-directory=", T.pack $ dropFileName toCompile, " ", T.pack toCompile,  " > /dev/null && convert -density 600x600 ", toCompilePdf, " -quality 90 -resize 800x600 ", T.pack target, " > /dev/null"])
        exitCode <- waitForProcess pHandle
        removeFile $ T.unpack toCompilePdf
        if exitCode == ExitSuccess then
            return ()
        else
            error "Failure to compile tikzpicture to PNG."
    where toCompilePdf = T.replace ".tex" ".pdf" (T.pack toCompile)


-- | Given content of tikzpicture environment, compile to PNG and return the filename.
-- Note that the caller should delete the file when finished with it.
tikzpictureToPng :: T.Text      -- ^ The contents of the tikzpicture environment.
                 -> IO FilePath -- ^ The name of the resulting PNG file.
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

        --  Remove temporary files.  Note we leave the PNG file for the Node process to clean up.
        let logFileName = T.unpack $ T.replace ".tex" ".log" (T.pack fileName)
            auxFileName = T.unpack $ T.replace ".tex" ".aux" (T.pack fileName)
        removeFile fileName
        removeFile logFileName
        removeFile auxFileName

        return pngFileName

tikzFilter :: T.Text -> Block -> IO Block
tikzFilter repoId b@(RawBlock (Format "latex") s) =
    let
        sT = T.pack s
    in 
        do
            if T.isPrefixOf ("\\begin{tikzpicture}") sT
            then
                do
                    pngFileName <- tikzpictureToPng sT

                    -- Hash PNG Contents, and use as id for image in new block returned.
                    pngContents <- B.readFile pngFileName
                    let h = abs $ hash pngContents

                    -- Write names of PNG files with identifiers to MongoDB; node process will then store in GridFS.
                    -- The Haskell MongoDB driver doesn't support GridFS, else this would just be a one step process.
                    addPngFileNameToMongo pngFileName h repoId
                    
                    return $ Plain [Image [] ("/tikzpictures/" ++ (show h), "Tikz Picture")]
            else
                return b
tikzFilter _ b = return b

-- TODO: Move Mongo host/credentials to environment variables.
addPngFileNameToMongo :: FilePath -> Int -> T.Text -> IO ()
addPngFileNameToMongo fileName h repoId =
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
            insert_ "TikzPngFilesToLoad" ["filename" =: fileName, "hash" =: h, "repoId" =: repoId]

-- | Turn latex RawBlocks for the given environment into Divs with that environment as their class.
-- Normally, these blocks are ignored by HTML writer. -}
environmentFilter :: T.Text -> Block -> Block
environmentFilter e b@(RawBlock (Format "latex") s) =
	let
		sLen = length s
		eLen = T.length e
	in 
		if T.isPrefixOf (T.concat ["\\begin{", e, "}"]) (T.pack s)
			then Div ("",[T.unpack e],[]) [Plain [Str $ drop (eLen + 8) $ take (sLen - (eLen + 6)) s]]
			else b
environmentFilter _ b = b

environmentFilters :: [Block -> Block]
environmentFilters = map environmentFilter environments

substituteRawBlocks :: (Map.Map String MetaValue) -> Block -> IO Block
substituteRawBlocks m x =
    do
        let repoId = findRepoId m
        let y = foldl (flip ($)) x environmentFilters
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
    --let repoId = findRepoId $ either error id . eitherDecode'
    toJSONFilterMeta substituteRawBlocks

-- Modified version of toJSONFilter, also passing metadata.
toJSONFilterMeta :: (Map.Map String MetaValue -> Block -> IO Block) -> IO ()
toJSONFilterMeta f =
    do
        jsonContents <- B.getContents
        let doc = either error id . eitherDecode' $ jsonContents
        let meta = case doc of
                       Pandoc m _ -> unMeta m
        processedDoc <- (walkM (f meta) :: Pandoc -> IO Pandoc) $ doc
        B.putStr . encode $ processedDoc
