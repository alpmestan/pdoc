module Store where

import Data.Map.Strict (Map)
import Data.Time (UTCTime)

import qualified Data.Map as Map

type DB = Map FilePath Entry

data Entry = Entry
    { entrySrc :: FilePath
    , entrySrcMTime :: UTCTime
    , entryTpl :: Maybe (FilePath, UTCTime)
    } deriving (Read, Show)

record :: FilePath -> FilePath -> UTCTime -> Maybe (FilePath, UTCTime) -> DB -> DB
record target src mtime mtpl = Map.insert target (Entry src mtime mtpl)

needsWork :: FilePath -> UTCTime -> Maybe (FilePath, UTCTime) -> DB -> Bool
needsWork target srcmtime mtpl db
    -- if we know about this target already, then we need to produce it again only
    -- when the source file's mtime is more recent than the one recorded in the DB,
    -- or the template file
    | Just e <- Map.lookup target db = entrySrcMTime e < srcmtime || tplConds e
    -- we don't know about this target, so we definitely need to do some work
    | otherwise = True

    where tplConds e = case (mtpl, entryTpl e) of
            (Nothing, Nothing) -> False
            (Nothing, Just _)  -> True
            (Just _, Nothing)  -> True
            (Just (fp1, mt1), Just (fp2, mt2))
                | fp1 /= fp2 -> True
                | mt1 > mt2  -> True
                | otherwise  -> False
            -- | Just (tpl, tplmtime) <- mtpl = entryTpl e /= tpl || entryTplMTime e < tplmtime
            -- | otherwise                    = False
