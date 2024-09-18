{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.FilePattern.Directory
import System.Directory
import Options.Applicative
import Control.Monad (forM_, when)
import Data.List (stripPrefix)
import System.FilePath ((</>), replaceExtension, takeDirectory, takeFileName)
import System.Process (callProcess)
import Data.IORef (newIORef, readIORef, atomicModifyIORef')

import Store (DB, record, needsWork)
import Text.Pandoc (Meta, Pandoc (..), ReaderOptions (..), readMarkdown, runIOorExplode, pandocExtensions, def, lookupMeta, MetaValue (..), Inline (Str))
import qualified Data.Text as T
import qualified Text.Pandoc.UTF8 as T
import Data.Maybe (fromMaybe, catMaybes)
import Text.Pandoc.Shared (trim)
import Data.Time (UTCTime)

data Opts = Opts
  { optsSrcDir  :: FilePath -- ^ root of source files for the site. /defaut: current directory/
  , optsOutDir  :: FilePath -- ^ where to put the generated site
  , optsTplsDir :: Maybe FilePath -- ^ where templtes can be found
  , optsTpl     :: Maybe String   -- ^ name of the default tenplate
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

data Format = HTML | PDF deriving Show

lookupFormats :: Meta -> [Format]
lookupFormats m = fromMaybe [HTML] $ do
    fstrs <- lkp "formats" m
    return $ catMaybes (map parseFormat fstrs)

    where lkp s m = lookupMeta s m >>= \mv -> case mv of
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

formatArg :: Format -> String
formatArg f = case f of
    HTML -> "html5+smart"
    PDF  -> "pdf"

formatExt :: Format -> String
formatExt f = case f of
    HTML -> "html"
    PDF -> "pdf"

getDocsInfo :: Opts -> [FilePath] -> IO [(FilePath, UTCTime, FilePath, Format, Maybe FilePath)]
getDocsInfo args fps = concat <$> traverse go fps
    where go fp = do
            mt <- getModificationTime fp
            meta <- readMeta fp
            let formats = lookupFormats meta
                tpl = lookupTemplate meta
            outs <- traverse (\f -> (,f) <$> makeAbsolute (outFile args fp f)) formats
            return [ (fp, mt, o, f, tpl) | (o, f) <- outs ]


main :: IO ()
main = do
    args <- execParser opts
    docs <- map (optsSrcDir args </>) <$> getDirectoryFiles (optsSrcDir args) ["**/*.md"]
    mtplsDir <- traverse makeAbsolute (optsTplsDir args)
    let defTpl = optsTpl args
    pdocd <- getPdocDir
    storepath <- getPdocStore
    pdocdb <- loadStore storepath
    pdocdbref <- newIORef pdocdb
    docsInfo <- getDocsInfo args docs
    let todoDocs = filter (\(_fp, time, out, _f, _tpl) -> needsWork out time pdocdb) docsInfo
    let ndocs = length todoDocs
    when (ndocs == 0) $ putStrLn "Nothing to do."
    forM_ (zip [(1::Int)..] todoDocs) $ \(i, (docPath, docMTime, outpath, format, mtpl)) -> do
        let pandocArgs = [ "--embed-resources"
                        , "--standalone"
                        , "--katex"
                        , "--from", "markdown"
                        , "--to", formatArg format
                        -- , "--filter", "pandoc-sidenote"
                        , "--filter", "pandoc-plot"
                        , "--filter", "pdoc-list"
                        , "--filter", "diagrams-pandoc"
                        , "-M", "plot-configuration=" ++ (pdocd </> "pandoc-plot.yml")
                        , "--toc"
                        , "--wrap=auto"
                        ] ++ (fromMaybe [] $
                                do tplsDir <- mtplsDir
                                   tpl <- mtpl <|> optsTpl args
                                   return ["--template", tplsDir </> tpl]
                             ) ++
                        [ "-o", outpath
                        , takeFileName docPath
                        ]
        createDirectoryIfMissing True (takeDirectory outpath)
        putStrLn $ "[" ++ show i ++ "/" ++ show ndocs ++ "] " ++ docPath ++ " -> " ++ outpath
        withCurrentDirectory (takeDirectory docPath) $ callProcess "pandoc" pandocArgs
        atomicModifyIORef' pdocdbref $ \db -> (record outpath docPath docMTime db, ())
    saveStore storepath =<< readIORef pdocdbref
