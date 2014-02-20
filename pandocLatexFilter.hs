
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
import Text.LaTeX.Base.Parser
import Text.LaTeX.Base.Render
import Text.LaTeX.Base.Syntax

data EnvironmentType = EnvDiv
                     | EnvDivNoContent
                     | JavascriptDiv
                     | PythonDiv
                     | MultipleChoice
                     | Abstract
                     | TikzPicture

data ActionType = Answer
                | EnvSpan
                | EnvSpanNoContent
                | CodeSpan
                | Choice
                | IncludeGraphics
                | ActivityTitle
                | ShortDescription

-- Type, classes, attributes
environmentMappings :: Map.Map T.Text (EnvironmentType, [String], [(String,String)])
environmentMappings = Map.fromList [
    ("abstract", (Abstract, [], [])),
    ("multiple-choice", (MultipleChoice, ["multiple-choice"], [("ximera-multiple-choice", "")])),
    ("tikzpicture", (TikzPicture, [], [])),
    ("python", (PythonDiv, ["python"], [("ximera-python", "")])),
    ("free-response", (EnvDiv, ["free-response"], [("ximera-free-response", "")])),
    ("matrix-answer", (JavascriptDiv, ["matrix-answer"], [("ximera-matrix-answer", "")])),
    ("expression-answer", (JavascriptDiv, ["expression-answer"], [("ximera-expression-answer", "")]))]

actionMappings :: Map.Map T.Text (ActionType, [String], [(String,String)])
actionMappings = Map.fromList [
    ("answer", (Answer, ["answer"], [("ximera-answer", "")])),
    ("choice", (Choice, ["choice"], [("ximera-choice", "")])),
    ("activitytitle", (ActivityTitle, [], [])),
    ("shortdescription", (ShortDescription, [], [])),
    ("includegraphics", (IncludeGraphics, [], [])),
    ("youtube", (EnvSpan, ["youtube"], [("ximera-youtube", "")]))]

environments :: [T.Text]
environments = Map.keys environmentMappings

actions :: [T.Text]
actions = Map.keys actionMappings


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
  pipe <- runIOE $ connect (readHostPort mongoHost)
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

writeMetaTitleToMongo :: Map.Map String MetaValue -> IO ()
writeMetaTitleToMongo meta  =
  case Map.lookup "title" meta of
        Just x -> writeTitleToMongo meta (showTitle x)
        _ -> return ()

writeTitleToMongo :: Map.Map String MetaValue -> String -> IO ()
writeTitleToMongo meta title =
  let hash = case Map.lookup "hash" meta of
        Just (MetaString x) -> x
        _ -> error "File hash not included in filter metadata."
      selectSt = Select {selector = ["baseFileHash" =: hash], coll = "activities"}
  in runMongo $ modify selectSt ["$set" =: ["title" =: title]]

writeLogToMongo :: Map.Map String MetaValue -> String -> IO ()
writeLogToMongo meta log =
  let hash = case Map.lookup "hash" meta of
        Just (MetaString x) -> x
        _ -> error "File hash not included in filter metadata."
      selectSt = Select {selector = [], coll = "activities"}
  in runMongo $ modify selectSt ["$set" =: ["log" =: log]]


stripDollars :: String -> String
stripDollars = takeWhile (not .(== '$')) . dropWhile (== '$')

-- Returns ([optional args], [required args])
parseTeXArgs :: [TeXArg] -> ([T.Text], [T.Text])
parseTeXArgs args = parseTeXArgs' args [] []
  where
    parseTeXArgs' [] opt req = (opt, req)
    parseTeXArgs' (x:xs) opt req =
      let
        (newOpt, newReq) = case x of
          FixArg y -> (opt, req ++ [render y])
          OptArg y -> (opt ++ [render y], req)
          MOptArg ys -> (opt ++ map render ys, req)
          otherwise -> (opt, req)
      in
        parseTeXArgs' xs newOpt newReq

imageFilter :: Map.Map String MetaValue -> Block -> IO Block
imageFilter meta (Para inlines) = do
  mappedInlines <- mapM (imageFilter' meta) inlines
  return $ Para mappedInlines
imageFilter _ b = return b

imageFilter' :: Map.Map String MetaValue -> Inline -> IO Inline
imageFilter' meta i@(Image _ (filename, _)) =
  let dotIndex = fromMaybe (error "No file extension for image") $ elemIndex '.' (reverse filename)
      extension = drop (length filename - dotIndex) filename
      imageMimeType = case extension of
        "png" -> "image/png"
        "jpg" -> "image/jpeg"
        "jpeg"-> "image/jpeg"
        "pdf" -> "image/png"
        _ -> error ("Unknown image type for " ++ extension)
  in do
    -- TODO: Sandbox so this can only read files from the current directory?
    h <- case extension of
      "pdf" -> do
        pdfContent <- compilePdfToPng $ T.pack filename
        addImageToMongo meta (T.pack imageMimeType) pdfContent
      _ -> do
        imageContent <- B.readFile filename
        addImageToMongo meta (T.pack imageMimeType) imageContent
    return $ Image [] ("/image/" ++ show h, "Included Graphic")
imageFilter' _ i = return i

mapInlineFilter :: (Map.Map String MetaValue -> Inline -> IO Inline) -> Map.Map String MetaValue -> Block -> IO Block
mapInlineFilter f meta (Para inlines) = do
  mappedInlines <- mapM (f meta) inlines
  return $ Para mappedInlines
mapInlineFilter _ _ b = return b

actionFilter  = mapInlineFilter actionFilter'

actionFilter' :: Map.Map String MetaValue -> Inline -> IO Inline
actionFilter' meta i@(RawInline (Format "latex") s) =
  case parseLaTeX (T.pack s) of
    Left errorString -> return i
    Right (TeXComm name args) -> inlineCommand (T.pack name) (parseTeXArgs args)
    Right (TeXCommS name) -> inlineCommand (T.pack name) ([], [])
  where
    inlineCommand :: T.Text -> ([T.Text], [T.Text]) -> IO Inline
    inlineCommand name (optionalParameters, requiredParameters) =
      let
        (inlineType, baseClasses, baseAttributes) = fromMaybe (EnvSpan, [T.unpack name], [("ximera-" ++ T.unpack name, "")]) (Map.lookup name actionMappings)
        content = case requiredParameters of
          (x:xs) -> T.unpack x
          [] -> ""
      in do
        randId <- nextRandom
        let attributes = (("data-uuid", toString randId) : baseAttributes) ++
                         zipWith (\i a -> ("data-optionalarg" ++ show i, T.unpack a)) [1,2..] optionalParameters ++
                         zipWith (\i a -> ("data-arg" ++ show i, T.unpack a)) [1,2..] requiredParameters
        let classes = baseClasses
        case inlineType of
          EnvSpanNoContent -> return $ Span ("", classes, attributes) []
          EnvSpan -> do
            blocks <- parseRawBlock content meta
            let inline = extractTopInline blocks
            return $ Span ("", classes, attributes) [inline]
          CodeSpan -> do
            let cdata = "<![CDATA[" ++ content ++  "]]>"
            return $ Span ("", classes, attributes) [Str cdata]
          Answer -> return $ Span ("", classes, attributes ++ [("data-answer", stripDollars content)]) []
          ShortDescription -> do
            writeDescriptionToMongo meta content
            return $ Str ""
          ActivityTitle -> do
            writeTitleToMongo meta content
            return $ Str ""
          Choice -> do
            -- TODO: Put correct/incorrect into this div from second argument.
            let value = case optionalParameters of
                  v:_ -> T.unpack v
                  _ -> ""
            return $ Span ("",classes, attributes ++ [("data-value",value)]) [Str content]
actionFilter' _ i = return i

environmentFilter :: Map.Map String MetaValue -> Block -> IO Block
environmentFilter meta b@(RawBlock (Format "latex") s) =
  case parseLaTeX (T.pack s) of
    Left errorString -> return b
    Right (TeXEnv name args latexContent) ->
      let
        tName = T.pack name
        (optionalParameters, requiredParameters) = parseTeXArgs args
        (envType, baseClasses, baseAttributes) = fromMaybe (EnvDiv, [name], [("ximera-" ++ name, "")]) (Map.lookup tName environmentMappings)
        content = T.unpack . render $ latexContent
      in do
        randId <- nextRandom
        let attributes = (("data-uuid", toString randId) : baseAttributes) ++
                         zipWith (\i a -> ("data-optionalarg" ++ show i, T.unpack a)) [1,2..] optionalParameters ++
                         zipWith (\i a -> ("data-arg" ++ show i, T.unpack a)) [1,2..] requiredParameters
        let classes = baseClasses
        case envType of
          EnvDivNoContent -> return $ Div ("", classes, attributes) []
          JavascriptDiv -> do
            let cdata = "<script type=\"text/javascript\">" ++ content ++ "</script>"
            return $ Div ("", classes, attributes) [RawBlock (Format "html") cdata]
          PythonDiv -> do
            let cdata = "<script type=\"text/python\">" ++ content ++ "</script>"
            return $ Div ("", classes, attributes) [RawBlock (Format "html") cdata]
          EnvDiv -> do
            blocks <- parseRawBlock content meta
            return $ Div ("", classes, attributes) blocks
          Abstract -> do
            writeDescriptionToMongo meta content
            return $ Plain []
          MultipleChoice -> do
            blocks <- parseRawBlock content meta
            return $ Div ("", classes, attributes ++ [("data-answer", "correct")]) blocks
          TikzPicture -> do
            pngContent <- tikzpictureToPng (T.pack content)
            -- Add file contents to MongoDB
            h <- addImageToMongo meta "image/png" pngContent
            return $ Plain [Image [] ("/image/" ++ show h, "Tikz Picture")]
    _ -> return b
environmentFilter _ b = return b

mathFilter = mapInlineFilter mathFilter'

mathFilter' :: Map.Map String MetaValue -> Inline -> IO Inline
mathFilter' _ (Math DisplayMath c) = return $ RawInline (Format "html") $ "<script type='math/tex'>" ++ c ++ "</script>"
mathFilter' _ (Math InlineMath c) = return $ RawInline (Format "html") $ "<script type='math/tex'>" ++ c ++ "</script>"
mathFilter' _ i = return i

parseRawBlock :: String -> Map.Map String MetaValue -> IO [Block]
parseRawBlock content meta =
    let
        (Pandoc _ blocks) = readLaTeX (def {readerParseRaw = True}) content
    in
        mapM (substituteRawBlocks meta) blocks

substituteRawBlocks :: Map.Map String MetaValue -> Block -> IO Block
substituteRawBlocks m x =
    foldM (flip ($)) x [environmentFilter m, actionFilter m, imageFilter m, mathFilter m]

extractTopInline :: [Block] -> Inline
extractTopInline (x:xs) = case extractTopInline' x of
  Just i -> i
  Nothing -> extractTopInline xs
  where
    extractTopInline' x = case x of
      Plain (y:ys) -> Just y
      Para (y:ys) -> Just y
      Header _ _ (y:ys) -> Just y
      Div _ ys -> Just (extractTopInline ys)
      _ -> Nothing
extractTopInline [] = Str ""

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
        writeMetaTitleToMongo meta
        processedDoc <- (walkM (f meta) :: Pandoc -> IO Pandoc) doc
        -- Merge inline commands with adjacent paragraphs
        let (Pandoc _ processedBlocks) = processedDoc
        let finalBlocks = mergeAdjacent processedBlocks
        -- TODO: Put some metadata blocks at the beginning with repoId, activity hash.  Pass activity hash from activity service.
        BL.putStr . encode $ Pandoc (Meta meta) finalBlocks

mergeAdjacent :: [Block] -> [Block]
mergeAdjacent (a@(Para i) : b@(Plain [s@(Span (_, classes, _) _)]) : xs) =
    if not (null (map T.pack classes `intersect` actions)) then
      Para (i ++ [s]):xs
    else
      a:mergeAdjacent (b:xs)
mergeAdjacent (a@(Plain [s@(Span (_, classes, _) _)]):b@(Para i):xs) =
    if not (null (map T.pack classes `intersect` actions)) then
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
