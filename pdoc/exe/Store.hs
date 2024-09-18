module Store where

import Data.Map.Strict (Map)
import Data.Time (UTCTime)
import Debug.Trace

import qualified Data.Map as Map

type DB = Map FilePath Entry

data Entry = Entry
    { entrySrc :: FilePath
    , entrySrcMTime :: UTCTime
    } deriving (Read, Show)

record :: FilePath -> FilePath -> UTCTime -> DB -> DB
record target src mtime = Map.insert target (Entry src mtime)

needsWork :: FilePath -> UTCTime -> DB -> Bool
needsWork target srcmtime db
    -- if we know about this target already, then we need to produce it again only
    -- when the source file's mtime is more recent than the one recorded in the DB
    | Just e <- Map.lookup target db = entrySrcMTime e < srcmtime
    -- we don't know about this target, so we definitely need to do some work
    | otherwise = True
