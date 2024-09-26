{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Main where
import Text.Pandoc.JSON
import System.FilePattern.Directory (getDirectoryFiles)
import System.FilePath ((</>), replaceExtension, dropExtension)
import qualified Data.Text as T
import System.Directory (makeRelativeToCurrentDirectory)
import Data.List (groupBy, intercalate, sortOn)
import Data.Function (on)
import Data.List.Split (splitOn)
import Text.Pandoc
import qualified Text.Pandoc.UTF8 as T
import Text.Pandoc.Shared (stringify)

main :: IO ()
main = toJSONFilter listFilter


mkLink :: T.Text -> T.Text -> [Block]
mkLink title target = [ Plain [ Link ("", [], []) [Str title] (target, "")
                              ]
                      ]

listFilter :: Block -> IO Block
listFilter (Para [Str "listPages", Space, Str dir]) = do
    dir' <- makeRelativeToCurrentDirectory (T.unpack dir)
    pages <- map (dir' </>) <$> getDirectoryFiles dir' ["**/*.md"]
    return $ renderDocTree (regroupFiles pages)
listFilter (Para [Str "listPagesDates", Space, Str dir]) = do
    dir' <- makeRelativeToCurrentDirectory (T.unpack dir)
    pages <- map (dir' </>) <$> getDirectoryFiles dir' ["*.md"]
    -- putStrLn $ "Files: " ++ show pages
    datedPages <- traverse go pages
    return $ renderDocList datedPages

    where go fp = do
            (title, date) <- getFileInfo fp
            return (fp, title, date)
listFilter b = do
    -- print b
    return b

readMeta :: FilePath -> IO Meta
readMeta fp = do
    s <- T.readFile fp
    Pandoc m _ <- runIOorExplode (readMarkdown ropts s)
    return m
    where ropts = def { readerStandalone = True, readerExtensions = pandocExtensions }

getFileInfo :: FilePath -> IO (T.Text, T.Text)
getFileInfo fp = do
    m <- readMeta fp
    t <- getField "title" m
    d <- getField "date" m
    return (t, d)
    -- case (lkp "date" m, lkp "title" m of
    --     Nothing -> error $ "getFileDate for " ++ fp ++ ": not date field?"
    --     Just d -> return (T.unpack d)

    where getField f m = case lookupMeta f m of
            Nothing -> error $ fp ++ ": can't find field " ++ T.unpack f ++ " in metadata"
            Just s  -> return (stringify s)

regroupFiles :: [FilePath] -> [DocTree FilePath]
regroupFiles xs = case regroup [] $ map (splitOn "/") xs of
    [DocNode _d ds] -> ds
    ds -> ds

renderDocList :: [(FilePath, T.Text, T.Text)] -> Block
renderDocList xs =
    BulletList [go fp t d | (fp, t, d) <- reverse (sortOn _3 xs) ]

    where go fp t d = mkLink (d <> " - " <> t)
                             (T.pack $ replaceExtension fp "html")

          _3 (_, _, x) = x

renderDocTree :: [DocTree FilePath] -> Block
renderDocTree = BulletList . map renderDocNode

renderDocNode :: DocTree FilePath -> [Block]
renderDocNode t = case t of
    DocLeaf a xs -> mkLink (T.pack $ dropExtension a) . T.pack $ replaceExtension (intercalate "/" xs) "html"
    DocNode a xs -> [Plain [Str (T.pack a)], renderDocTree xs]

data DocTree a = DocNode a [DocTree a] | DocLeaf a [a]
    deriving Show

regroup :: (Eq a, Show a) => [a] -> [[a]] -> [DocTree a]
regroup _pref [] = []
regroup pref xs = case groupBy ((==) `on` headMaybe) xs of
    []       -> error "wtf1"
    groups -> map (goGroup pref) $ filter (/= [[]]) groups

goGroup :: (Eq a, Show a) => [a] -> [[a]] -> DocTree a
goGroup pref g = case g of
    (root:[]):_ -> DocLeaf root (pref ++ [root])
    (root:_):_ -> DocNode root $ regroup (pref ++ [root]) (map tail g)
    a          -> error ("wtf2: " ++ show a)

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x:_) = Just x
