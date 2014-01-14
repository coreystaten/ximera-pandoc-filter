
{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

import Prelude hiding (catch)

import Data.List
import Data.Maybe
import Control.Monad
import Text.Pandoc
import Text.Pandoc.Walk
import System.Exit (ExitCode(..))
import System.IO
import System.Environment
import System.Directory (getTemporaryDirectory, removeFile)
import System.FilePath.Posix (dropFileName, (</>))
import System.Process (runCommand, waitForProcess)
import Data.Aeson
import Data.Hashable
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Database.MongoDB hiding (lookup, replace, runCommand)
import Data.UUID (toString)
import Data.UUID.V4 (nextRandom)
import Text.Regex.PCRE

data EnvironmentType = EnvDiv | EnvSpan | AnswerSpan | ChoiceDiv | MultipleChoiceDiv | DescriptionMeta | IncludeGraphicsDiv | TikzPictureDiv

-- Type, classes, attributes
environmentMappings :: Map.Map T.Text (EnvironmentType, [String], [(String,String)])
environmentMappings = Map.fromList [
    ("shuffle", (EnvDiv, ["shuffle"], [("ximera-shuffle", "")])),
    ("question", (EnvDiv, ["question"], [("ximera-question", ""), ("shuffleStatus", "shuffleStatus")])),
    ("exercise", (EnvDiv, ["exercise"], [("ximera-exercise", ""), ("shuffleStatus", "shuffleStatus")])),
    ("exploration", (EnvDiv, ["exploration"], [("ximera-exploration", ""), ("shuffleStatus", "shuffleStatus")])),
    ("solution", (EnvDiv, ["solution"], [("ximera-solution", "")])),
    ("abstract", (DescriptionMeta, ["description"], [("ximera-description", "")])),
    ("answer", (AnswerSpan, ["answer"], [("ximera-answer", "")])),
    ("youtube", (EnvDiv, ["youtube"], [("ximera-youtube", "")])),
    ("answer", (AnswerSpan, ["answer"], [("ximera-answer", "ximera-answer")])),
    ("choice", (ChoiceDiv, ["choice"], [("ximera-choice", "")])),
    ("multiple-choice", (MultipleChoiceDiv, ["multiple-choice"], [("ximera-multiple-choice", "")])),
    ("includegraphics", (IncludeGraphicsDiv, [], [])),
    ("tikzpicture", (TikzPictureDiv, [], []))]

environments :: [T.Text]
environments = Map.keys environmentMappings

-- List of environments coming from inline commands; require parsing of arguments.
inlineEnvironments :: [T.Text]
inlineEnvironments = ["choice", "answer", "includegraphics"]

-- | The template to use for tikzpictures from the filter, loaded from tikz-template.tex
tikzTemplate :: IO Template
tikzTemplate =
    do
        filterPath <- getEnv "XIMERA_FILTER_PATH"
        let templatePath = dropFileName filterPath </> "tikz-template.tex"
        templateContents <- readFile templatePath
        let result =  case compileTemplate $ T.pack templateContents of
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
        -- TODO: Don't forget about security of
        -- running other people's pdflatex.  Either sandboxed version, or run
        -- this filter with low permissions.  Security plan: Run the
        -- pandoc/filter process using a Linux container or other sandbox; do
        -- not acces MongoDB from the filter, but instead move that to Node
        -- service (since sandbox should disable network access); disable first
        -- line configuration of LaTeX files (so user can not enable \write18);
        -- make sure repos are extracted to distinct directories so that
        -- sandboxing does not share repo content.
        pHandle <- runCommand $ T.unpack (T.concat ["pdflatex -output-directory=", T.pack $ dropFileName toCompile, " ", T.pack toCompile,  " > /dev/null && mudraw -o ", T.pack target, " ", toCompilePdf, " > /dev/null"])
        exitCode <- waitForProcess pHandle
        removeFile $ T.unpack toCompilePdf
        unless (exitCode == ExitSuccess) (error "Failure to compile tikzpicture to PNG.")
    where toCompilePdf = T.replace ".tex" ".pdf" (T.pack toCompile)

compilePdfToPng :: T.Text -- ^ The pdf file name.
                -> IO B.ByteString -- ^ The content of the resulting PNG file
compilePdfToPng pdfFilename = do
  pHandle <- runCommand $ T.unpack (T.concat ["mudraw -o ", pngFilename, " ", pdfFilename])
  exitCode <- waitForProcess pHandle
  unless (exitCode == ExitSuccess) (error "Failure to compile PDF to PNG.")
  B.readFile $ T.unpack pngFilename
  where pngFilename = T.replace ".pdf" ".png" pdfFilename

-- | Given content of tikzpicture environment, compile to PNG and return the contents.
tikzpictureToPng :: T.Text      -- ^ The contents of the tikzpicture environment.
                 -> IO B.ByteString -- ^ The content of the resulting PNG file.
tikzpictureToPng content =
    do
        -- Render through Pandoc template.
        template <- tikzTemplate
        let renderedTex = renderTemplate template $ object ["picture" .= content]
        -- Write LaTeX to temporary file.
        tempDir <- getTemporaryDirectory
        (fileName, handle) <- openTempFile tempDir "tikz.tex"
        hPutStr handle renderedTex
        hClose handle

        -- Compile PNG
        let pngFileName = T.unpack $ T.replace ".tex" ".png" (T.pack fileName)
        compileTikzFile fileName pngFileName

        B.readFile pngFileName



runMongo :: Action IO a -> IO ()
runMongo run = do
  mongoHost <- getEnv "XIMERA_MONGO_URL"
  mongoDatabase <- getEnv "XIMERA_MONGO_DATABASE"
  pipe <- runIOE $ connect (host mongoHost)
  err <- access pipe master (T.pack mongoDatabase) run
  case err of
    Left errStr -> error (show errStr)
    Right _ -> close pipe


addImageToMongo :: Map.Map String MetaValue -> T.Text -> B.ByteString -> IO Int
addImageToMongo meta mimetype content =
  let repoId = case Map.lookup "repoId" meta of
        Just (MetaString x) -> x
        _ -> error "repoID not included in filter metadata."
      h = abs $ hash content
  in do
    runMongo $ repsert Select {selector = ["hash" =: h], coll = "imageFiles"} ["content" =: Binary content, "hash" =: h, "repo" =: repoId, "mimetype" =: mimetype]
    return h

writeDescriptionToMongo :: Map.Map String MetaValue -> String -> IO ()
writeDescriptionToMongo meta description =
  let hash = case Map.lookup "hash" meta of
        Just (MetaString x) -> x
        _ -> error "File hash not included in filter metadata."
      selectSt = Select {selector = ["baseFileHash" =: hash], coll = "activities"}
  in runMongo $ modify selectSt ["$set" =: ["description" =: description]]

writeTitleToMongo :: Map.Map String MetaValue -> IO ()
writeTitleToMongo meta  =
  let hash = case Map.lookup "hash" meta of
        Just (MetaString x) -> x
        _ -> error "File hash not included in filter metadata."
      title = case Map.lookup "title" meta of
        Just x -> showTitle x
        _ -> error "Title not included in file metadata."
      selectSt = Select {selector = ["baseFileHash" =: hash], coll = "activities"}
  in runMongo $ modify selectSt ["$set" =: ["title" =: title]]

writeLogToMongo :: Map.Map String MetaValue -> String -> IO ()
writeLogToMongo meta log =
  let hash = case Map.lookup "hash" meta of
        Just (MetaString x) -> x
        _ -> error "File hash not included in filter metadata."
      selectSt = Select {selector = [], coll = "activities"}
  in runMongo $ modify selectSt ["$set" =: ["log" =: log]]


-- | Turn latex RawBlocks for the given environment into Divs with that environment as their class.
-- Normally, these blocks are ignored by HTML writer. -}
environmentFilter :: T.Text -> Map.Map String MetaValue -> Block -> IO Block
environmentFilter e meta b@(RawBlock (Format "latex") s) =
  let sLen = length s
      eLen = T.length e
  in if T.concat ["\\begin{", e, "}"] `T.isPrefixOf` T.pack s then
       let rawContent = drop (eLen + 8) $ take (sLen - (eLen + 6)) s
           isInline = elem e inlineEnvironments
           optionalParameters = parseOptionalParameters rawContent
           requiredParameters = if isInline then parseInlineRequiredParameters rawContent else [rawContent]
           -- Convenience variable for environments not using required parameters.
           content = head requiredParameters
           (envType, baseClasses, baseAttributes) = fromMaybe (error "This shouldn't happen: couldn't find environment in environmentMappings.") (Map.lookup e environmentMappings)
       in do
         randId <- nextRandom
         let attributes = ("data-uuid", toString randId) : baseAttributes
         let classes = baseClasses
         case envType of
           EnvDiv -> do
             blocks <- parseRawBlock content meta
             return $ Div ("", classes, attributes) blocks
           EnvSpan -> return $ Plain [Span ("", classes, attributes) [Str content]]
           AnswerSpan -> return $ Plain [Span ("", classes, attributes ++ [("data-answer", content)]) []]
           DescriptionMeta -> do
             writeDescriptionToMongo meta content
             return $ Div ("", classes, attributes) [Plain [Str content]]
           MultipleChoiceDiv -> do
             blocks <- parseRawBlock content meta
             return $ Div ("", classes, attributes ++ [("data-answer", "correct")]) blocks
           ChoiceDiv -> do
             -- TODO: Put correct/incorrect into this div from second argument.
             let value = case optionalParameters of
                   v:_ -> v
                   _ -> ""
             return $ Div ("",classes, attributes ++ [("data-value",value)]) [Plain [Str content]]
           TikzPictureDiv -> do
             pngContent <- tikzpictureToPng (T.pack content)
             -- Add file contents to MongoDB
             h <- addImageToMongo meta "image/png" pngContent
             return $ Plain [Image [] ("/image/" ++ show h, "Tikz Picture")]
           IncludeGraphicsDiv -> do
             let filename = content
                 dotIndex = fromMaybe (error "No file extension for image") $ elemIndex '.' (reverse filename)
                 extension = drop (length content - dotIndex) filename
             let imageMimeType = case extension of
                   "png" -> "image/png"
                   "jpg" -> "image/jpeg"
                   "jpeg"-> "image/jpeg"
                   "pdf" -> "image/png"
                   _ -> error ("Unknown image type for " ++ extension)
             -- TODO: Sandbox so this can only read files from the current directory?
             h <- case extension of
               "pdf" -> do
                 pdfContent <- compilePdfToPng $ T.pack filename
                 addImageToMongo meta (T.pack imageMimeType) pdfContent
               _ -> do
                 imageContent <- B.readFile content
                 addImageToMongo meta (T.pack imageMimeType) imageContent
             return $ Plain [Image [] ("/image/" ++ show h, "Included Graphic")]
     else return b
environmentFilter _ _ b = return b

-- Helper methods; type checking doesn't want to work without this...
-- Gives list of [match, matchGroup1, matchGroup2, ...]
pat :: String -> String -> [[String]]
pat pattern str = str =~ pattern

--Gives tuple of (beforeMatch, match, afterMatch)
patTuple :: String -> String -> (String, String, String)
patTuple pattern str = str =~ pattern

-- Example: "{asdf}{qwer}" -> ["asdf", "qwer"]


parseInlineRequiredParameters :: String -> [String]
parseInlineRequiredParameters content = map (!! 1) (pat "{([^}]*)}" content)

optionalParameterPattern = "^\\[([^\\]]*)\\]"

parseOptionalParameters :: String -> [String]
parseOptionalParameters content = map (!! 1) (pat optionalParameterPattern content)

removeOptionalParameters :: String -> String
removeOptionalParameters (patTuple optionalParameterPattern -> (_, _, remainder)) = remainder

parseRawBlock :: String -> Map.Map String MetaValue -> IO [Block]
parseRawBlock content meta =
    let
        (Pandoc _ blocks) = readLaTeX (def {readerParseRaw = True}) content
    in
        mapM (substituteRawBlocks meta) blocks

environmentFilters :: [Map.Map String MetaValue -> Block -> IO Block]
environmentFilters = map environmentFilter environments

substituteRawBlocks :: Map.Map String MetaValue -> Block -> IO Block
substituteRawBlocks m x =
    foldM (flip ($)) x (map ($ m) environmentFilters)

findRepoId :: Map.Map String MetaValue -> T.Text
findRepoId m =
    case Map.lookup "repoId" m of
        Just v -> case v of
                      MetaString s -> T.pack s
                      _ -> error "No repo ID passed to filter"
        Nothing -> error "No repo ID passed to filter"

main :: IO ()
main = toJSONFilterMeta substituteRawBlocks

-- Modified version of toJSONFilter, also passing metadata.
toJSONFilterMeta :: (Map.Map String MetaValue -> Block -> IO Block) -> IO ()
toJSONFilterMeta f =
    do
        jsonContents <- BL.getContents
        let doc = either error id . eitherDecode' $ jsonContents
        let meta = case doc of
                       Pandoc m _ -> unMeta m
        writeLogToMongo meta (show meta) -- TODO: Remove
        writeTitleToMongo meta
        processedDoc <- (walkM (f meta) :: Pandoc -> IO Pandoc) doc
        -- Merge inline commands with adjacent paragraphs
        let (Pandoc _ processedBlocks) = processedDoc
        let finalBlocks = mergeAdjacent processedBlocks
        -- TODO: Put some metadata blocks at the beginning with repoId, activity hash.  Pass activity hash from activity service.
        BL.putStr . encode $ Pandoc (Meta meta) finalBlocks

mergeAdjacent :: [Block] -> [Block]
mergeAdjacent (a@(Para i) : b@(Plain [s@(Span (_, classes, _) _)]) : xs) =
    if not (null (map T.pack classes `intersect` inlineEnvironments)) then
      Para (i ++ [s]):xs
    else
      a:mergeAdjacent (b:xs)
mergeAdjacent (a@(Plain [s@(Span (_, classes, _) _)]):b@(Para i):xs) =
    if not (null (map T.pack classes `intersect` inlineEnvironments)) then
      Para (s:i):xs
    else
      a:mergeAdjacent (b:xs)
mergeAdjacent (a:xs) = mergeInElement a : mergeAdjacent xs
mergeAdjacent [] = []

mergeInElement :: Block -> Block
mergeInElement (Div a xs) = Div a $ mergeAdjacent xs
mergeInElement x = x


-- Print title from meta value.  metaValueToJSON is hidden for some reason, so we can't use it.
showTitle :: MetaValue -> String
showTitle (MetaInlines xs) = showTitle' "" xs
  where showTitle' s (x:xs) = case x of
          Str s' -> showTitle' (s ++ s') xs
          Space -> showTitle' (s ++ " ") xs
          _ -> showTitle' "Error parsing title" xs
        showTitle' s [] = s
showTitle (MetaString x) = x
showtitle _ = "Error parsing title."
