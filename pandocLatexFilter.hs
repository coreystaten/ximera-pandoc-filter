
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

data EnvironmentType = EnvDiv | EnvSpan | AnswerSpan | ChoiceDiv | MultipleChoiceDiv | DescriptionMeta

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
    ("multiple-choice", (MultipleChoiceDiv, ["multiple-choice"], [("ximera-multiple-choice", "")]))]

environments :: [T.Text]
environments = Map.keys environmentMappings

-- List of environments coming from inline commands; require parsing of arguments.
inlineEnvironments :: [T.Text]
inlineEnvironments = ["choice", "answer", "activitytitle", "headline"]

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
        pHandle <- runCommand $ T.unpack (T.concat ["pdflatex -output-directory=", T.pack $ dropFileName toCompile, " ", T.pack toCompile,  " > /dev/null && convert -density 600x600 ", toCompilePdf, " -quality 90 -resize 800x600 ", T.pack target, " > /dev/null"])
        exitCode <- waitForProcess pHandle
        removeFile $ T.unpack toCompilePdf
        unless (exitCode == ExitSuccess) (error "Failure to compile tikzpicture to PNG.")
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
        tempDir <- getTemporaryDirectory
        (fileName, handle) <- openTempFile tempDir "tikz.tex"
        hPutStr handle renderedTex
        hClose handle

        -- Compile PNG
        let pngFileName = T.unpack $ T.replace ".tex" ".png" (T.pack fileName)
        compileTikzFile fileName pngFileName

        B.readFile pngFileName


tikzFilter :: T.Text -> Block -> IO Block
tikzFilter repoId b@(RawBlock (Format "latex") s) =
  let sT = T.pack s
  in if "\\begin{tikzpicture}" `T.isPrefixOf` sT then
       do pngContent <- tikzpictureToPng sT
          -- Hash PNG Contents, and use as id for image in new block returned.
          let h = abs $ hash pngContent

          -- Add file contents to MongoDB
          addPngFileToMongo pngContent h repoId
          return $ Plain [Image [] ("/tikzpictures/" ++ show h, "Tikz Picture")]
     else
       return b
tikzFilter _ b = return b

runMongo :: Action IO a -> IO ()
runMongo run = do
  mongoHost <- getEnv "XIMERA_MONGO_URL"
  mongoDatabase <- getEnv "XIMERA_MONGO_DATABASE"
  pipe <- runIOE $ connect (host mongoHost)
  err <- access pipe master (T.pack mongoDatabase) run
  case err of
    Left errStr -> error (show errStr)
    Right _ -> close pipe


addPngFileToMongo :: B.ByteString -> Int -> T.Text -> IO ()
addPngFileToMongo content h repoId =
    runMongo $ insert_ "tikzPngFiles" ["content" =: Binary content, "hash" =: h, "repoId" =: repoId]

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
             return $ Plain [Span ("", classes, attributes) [Str content]]
           MultipleChoiceDiv -> do
             blocks <- parseRawBlock content meta
             return $ Div ("", classes, attributes ++ [("data-answer", "correct")]) blocks
           ChoiceDiv -> do
             -- TODO: Put correct/incorrect into this div from second argument.
             let value = case optionalParameters of
                   v:_ -> v
                   _ -> ""
             return $ Div ("",classes, attributes ++ [("data-value",value)]) [Plain [Str content]]
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
    do
        let repoId = findRepoId m
        y <- foldM (flip ($)) x (map ($ m) environmentFilters)
        tikzFilter repoId y

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
        let finalBlocks = (unwrapBlocks . mergePList . wrapBlocks) processedBlocks
        -- TODO: Put some metadata blocks at the beginning with repoId, activity hash.  Pass activity hash from activity service.
        BL.putStr . encode $ Pandoc (Meta meta) finalBlocks


-- Wrap in an extra div so top level can be accessed as block.
wrapBlocks :: [Block] -> [Block]
wrapBlocks xs = [Div ("", [], []) xs]

unwrapBlocks :: [Block] -> [Block]
unwrapBlocks [Div ("", [], []) xs] = xs
unwrapBlocks x = x

mergePList :: [Block] -> [Block]
mergePList (x:xs) = foldM mergeAdjacent x xs

mergeAdjacent :: Block -> Block -> [Block]
mergeAdjacent a@(Para i) b@(Plain [s@(Span (_, classes, _) _)]) =
    if not (null (map T.pack classes `intersect` inlineEnvironments)) then
        [Para (i ++ [s])]
    else
        [a, b]
mergeAdjacent a@(Plain [s@(Span (_, classes, _) _)]) b@(Para i) =
    if not (null (map T.pack classes `intersect` inlineEnvironments)) then
        [Para (s:i)]
    else
        [a, b]
mergeAdjacent a b = [a,b]


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
