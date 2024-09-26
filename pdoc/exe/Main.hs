{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.FilePattern.Directory
import System.Directory
import Options.Applicative
import Control.Monad (forM_, when)
import Data.List (stripPrefix)
import System.FilePath ((</>), replaceExtension, takeDirectory, takeFileName, (<.>))
import System.Process (callProcess)
import Data.IORef (newIORef, readIORef, atomicModifyIORef')

import Store (DB, record, needsWork)
import Text.Pandoc (Meta, Pandoc (..), ReaderOptions (..), readMarkdown, runIOorExplode, pandocExtensions, def, lookupMeta, MetaValue (..), Inline (Str))
import qualified Data.Text as T
import qualified Text.Pandoc.UTF8 as T
import Data.Maybe (fromMaybe, catMaybes)
import Text.Pandoc.Shared (trim)
import Data.Time (UTCTime)
import Debug.Trace (traceShow)
import Text.Read (readMaybe)
import Data.Function ((&))

data Opts = Opts
  { optsSrcDir  :: FilePath -- ^ root of source files for the site. /defaut: current directory/
  , optsOutDir  :: FilePath -- ^ where to put the generated site
  , optsTplsDir :: Maybe FilePath -- ^ where templtes can be found
  , optsTpl     :: Maybe String   -- ^ name of the default tenplate
  , optsVerbose :: Bool -- ^ print additional information to stdout
  } deriving Show

parseOpts :: Parser Opts
parseOpts = Opts
        <$> strOption
              ( long "input"
             <> short 'i'
             <> metavar "DIR"
             <> value "."
             <> showDefault
             <> help "Root directory of the source documents for the site"
              )
        <*> strOption
              ( long "out"
             <> short 'o'
             <> metavar "DIR"
             <> showDefault
             <> help "Directory where the generated site will be placed"
              )
        <*> optional (strOption
              ( long "tpls"
             <> short 'd'
             <> metavar "DIR"
             <> showDefault
             <> help "Directory in which templates can be found"
              ))
        <*> optional (strOption
              ( long "tpl"
             <> short 't'
             <> metavar "NAME"
             <> showDefault
             <> help "Name of the template to use"
              ))
        <*> switch
              ( long "verbose"
             <> short 'v'
             <> help "Display additional information about what pdoc is doing."
              )

opts :: ParserInfo Opts
opts = info (parseOpts <**> helper)
      ( fullDesc
     <> progDesc "A pandoc-based website build system"
     <> header "pdoc - pandoc build system"
      )

getPdocDir :: IO FilePath
getPdocDir = do
    d <- (</> ".pdoc") <$> getHomeDirectory
    createDirectoryIfMissing True d
    writeFile (d </> "pandoc-plot.yml") (plotYml d)
    return d

getPdocStore :: IO FilePath
getPdocStore = (</> ".pdoc.db") <$> getCurrentDirectory

loadStore :: FilePath -> IO DB
loadStore fp = do
    exists <- doesFileExist fp
    if exists
        then read <$> readFile fp
        else return mempty

saveStore :: FilePath -> DB -> IO ()
saveStore fp = writeFile fp . show

plotYml :: FilePath -> String
plotYml pdocd = unlines
    [ "directory: " ++ (pdocd </> "plots")
    , "format: PNG"
    , "matplotlib:"
    , "  transparent: true"
    , "  tight_bbox: true"
    , "d2:"
    , "  command_line_arguments: --sketch --scale 1 --dark-theme 200"
    ]

rebase :: Opts -> FilePath -> FilePath
rebase args s
    | Just s' <- stripPrefix (optsSrcDir args) s = optsOutDir args </> tail s'
    | otherwise = error $ "wtf: " ++ show (args, s)

outFile :: Opts -> FilePath -> Format -> FilePath
outFile args fp f = replaceExtension (rebase args fp) (formatExt f)

readMeta :: FilePath -> IO Meta
readMeta fp = do
    s <- T.readFile fp
    Pandoc m _ <- runIOorExplode (readMarkdown ropts s)
    return m
    where ropts = def { readerStandalone = True, readerExtensions = pandocExtensions }

data Format = HTML | PDF deriving (Eq, Show)

lookupFormats :: Meta -> [Format]
lookupFormats m = fromMaybe [HTML] $ do
    fstrs <- lkp "formats"
    return $ catMaybes (map parseFormat fstrs)

    where lkp s = lookupMeta s m >>= \mv -> case mv of
            MetaInlines strs -> Just strs
            _ -> Nothing
          parseFormat (Str "html") = Just HTML
          parseFormat (Str "pdf") = Just PDF
          parseFormat _ = Nothing

lookupTemplate :: Meta -> Maybe FilePath
lookupTemplate m = T.unpack <$> lkp "template"
    where lkp s = lookupMeta s m >>= \mv -> case mv of
            MetaInlines [Str s] -> Just s
            _ -> Nothing

lookupTocDepth :: Meta -> Int
lookupTocDepth m = fromMaybe 3 $ do
    depthStr <- T.unpack <$> lkp "toc-depth"
    readMaybe depthStr

    where lkp s = lookupMeta s m >>= \mv -> case mv of
            MetaInlines [Str s] -> Just s
            _ -> traceShow mv Nothing

lookupTocNums :: Meta -> Bool
lookupTocNums m = lkp "toc-nums"
    where lkp s = lookupMeta s m & \mv -> case mv of
            Just (MetaBool b) -> b
            _ -> False

formatArg :: Format -> String
formatArg f = case f of
    HTML -> "html5+smart"
    PDF  -> "pdf"

formatExt :: Format -> String
formatExt f = case f of
    HTML -> "html"
    PDF -> "pdf"

formatTplExt :: Format -> String
formatTplExt f = case f of
    HTML -> "html5"
    PDF -> "latex"

getDocsInfo :: Opts -> Maybe FilePath -> [FilePath] -> IO [(FilePath, UTCTime, FilePath, Format, Maybe (FilePath, UTCTime), Int, Bool)]
getDocsInfo args mtplsDir fps = concat <$> traverse go fps
    where go fp = do
            mt <- getModificationTime fp
            meta <- readMeta fp
            let formats = lookupFormats meta
                mtpl = lookupTemplate meta
                tocDepth = lookupTocDepth meta
                numbered = lookupTocNums meta
            let mtpl' = (</>) <$> mtplsDir <*> (mtpl <|> optsTpl args)
            outs <- traverse (goFormat fp mtpl') formats
            return [ (fp, mt, o, f, tplf, tocDepth, numbered) | (f, o, tplf) <- outs ]
          goFormat fp mtpl format = do
            o <- makeAbsolute (outFile args fp format)
            mtpl' <- flip traverse mtpl $ \tplBasePath -> do
                let tplPath = tplBasePath <.> formatTplExt format
                tplmt <- getModificationTime tplPath
                return (tplPath, tplmt)
            return (format, o, mtpl')

main :: IO ()
main = do
    args <- execParser opts
    docs <- map (optsSrcDir args </>) <$> getDirectoryFiles (optsSrcDir args) ["**/*.md"]
    mtplsDir <- traverse makeAbsolute (optsTplsDir args)
    pdocd <- getPdocDir
    storepath <- getPdocStore
    pdocdb <- loadStore storepath
    pdocdbref <- newIORef pdocdb
    docsInfo <- getDocsInfo args mtplsDir docs
    let todoDocs = filter (\(_fp, time, out, _f, mtpl, _td, _num) -> needsWork out time mtpl pdocdb) docsInfo
    let ndocs = length todoDocs
    when (ndocs == 0) $ putStrLn "Nothing to do."
    forM_ (zip [(1::Int)..] todoDocs) $ \(i, (docPath, docMTime, outpath, format, mtpl, tocDepth, numbered)) -> do
        let pandocArgs = [ "--embed-resources"
                        , "--standalone"
                        , "--katex"
                        , "--from", "markdown"
                        , "--to", formatArg format
                        , "--filter", "pandoc-plot"
                        , "--filter", "pdoc-list"
                        , "--filter", "diagrams-pandoc"
                        , "--filter", "pandoc-sidenote"
                        , "-M", "plot-configuration=" ++ (pdocd </> "pandoc-plot.yml")
                        , "--toc"
                        , "--toc-depth=" ++ show tocDepth
                        ] ++
                        [ "-N" | numbered ] ++
                        concat [ [ "-f", "markdown-implicit_figures" ] | format == PDF ] ++
                        [ "--wrap=auto"
                        ] ++ (fromMaybe [] $
                                do (tplPath, _) <- mtpl
                                   return ["--template", tplPath]
                             ) ++
                        [ "-o", outpath
                        , takeFileName docPath
                        ]
        createDirectoryIfMissing True (takeDirectory outpath)
        putStrLn $ "[" ++ show i ++ "/" ++ show ndocs ++ "] " ++ docPath ++ " -> " ++ outpath
        when (optsVerbose args) $ putStrLn $
            ">> pandoc " ++ unwords pandocArgs
        withCurrentDirectory (takeDirectory docPath) $ callProcess "pandoc" pandocArgs
        atomicModifyIORef' pdocdbref $ \db -> (record outpath docPath docMTime mtpl db, ())
    saveStore storepath =<< readIORef pdocdbref
