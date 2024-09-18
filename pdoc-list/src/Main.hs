{-# LANGUAGE OverloadedStrings #-}
module Main where
import Text.Pandoc.JSON
import System.FilePattern.Directory (getDirectoryFiles)
import System.FilePath ((</>), replaceExtension, dropExtension)
import qualified Data.Text as T
import System.Directory (makeRelativeToCurrentDirectory)
import Data.List (groupBy, intercalate)
import Data.Function (on)
import Data.List.Split (splitOn)

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
listFilter b = do
    -- print b
    return b

regroupFiles :: [FilePath] -> [DocTree FilePath]
regroupFiles xs = case regroup [] $ map (splitOn "/") xs of
    [DocNode _d ds] -> ds
    ds -> ds

renderDocTree :: [DocTree FilePath] -> Block
renderDocTree = BulletList . map renderDocNode

renderDocNode :: DocTree FilePath -> [Block]
renderDocNode t = case t of
    DocLeaf a xs -> mkLink (T.pack $ dropExtension a) . T.pack $ replaceExtension (intercalate "/" xs) "html"
    DocNode a xs -> [Plain [Str (T.pack a)], renderDocTree xs]

data DocTree a = DocNode a [DocTree a] | DocLeaf a [a]
    deriving Show

regroup :: (Show a, Ord a) => [a] -> [[a]] -> [DocTree a]
regroup _pref [] = []
regroup pref xs = case groupBy ((==) `on` headMaybe) xs of
    []       -> error "wtf1"
    groups -> map (goGroup pref) $ filter (/= [[]]) groups

goGroup :: (Show a, Ord a) => [a] -> [[a]] -> DocTree a
goGroup pref g = case g of
    (root:[]):_ -> DocLeaf root (pref ++ [root])
    (root:_):_ -> DocNode root $ regroup (pref ++ [root]) (map tail g)
    a          -> error ("wtf2: " ++ show a)

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x:_) = Just x
