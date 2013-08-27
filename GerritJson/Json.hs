
module GerritJson.Json (
    GerritJsonRes(..)
  , Stats(..)
  , Change(..)
  , Approval(..)
  , Rev(..)
  , Author(..)
  , ReviewType(..)
  , decodeGerritJson
  ) where

import Control.Applicative
import Data.Time (UTCTime)
import Data.Time.Format (readTime)
import System.Locale (defaultTimeLocale)
import Text.JSON

data GerritJsonRes = GerritJsonC Change | GerritJsonStats Stats deriving (Eq, Show)

data Stats = Stats Int deriving (Eq, Show)

data Change = Change {
    changeId :: Int
  , url :: String
  , project :: String
  , branch :: String
  , sortKey :: String
  , owner :: Author
  , revs :: [Rev]
  } deriving (Eq, Show)

instance Ord Change where
  (<=) a b = changeId a <= changeId b

data Author = Author {
    name :: String
  , email :: Maybe String
  } deriving (Eq, Ord, Show)

data ReviewType = Crvw | Subm | Vrif | User String deriving (Eq, Show, Ord)

data Approval = Approval {
    approvalType :: ReviewType
  , value :: Int
  , grantedOn :: UTCTime
  , approver :: Author
  } deriving (Eq, Show)

data Rev = Rev {
    revId :: Int
  , uploader :: Author
  , approvals :: [Approval]
  } deriving (Eq, Show)

instance JSON Author where
  readJSON object = do
    obj <- readJSON object
    Author <$> valFromObj "name" obj
           <*> (valFromObj "email" obj <|> pure Nothing)
  showJSON _ = error "unimplemented"

toApproval :: String -> ReviewType
toApproval "CRVW"        = Crvw
toApproval "Code-Review" = Crvw
toApproval "SUBM"        = Subm
toApproval "VRIF"        = Vrif
toApproval "Verified"    = Vrif
toApproval x             = User x

fromUnixTime :: Int -> UTCTime
fromUnixTime = readTime defaultTimeLocale "%s" . show

instance JSON Approval where
  readJSON object = do
    obj <- readJSON object
    Approval <$> (toApproval <$> valFromObj "type" obj)
             <*> (read <$> valFromObj "value" obj)
             <*> (fromUnixTime <$> valFromObj "grantedOn" obj)
             <*> valFromObj "by" obj
  showJSON _ = error "unimplemented"

instance JSON Rev where
  readJSON object = do
    obj <- readJSON object
    Rev <$> read <$> valFromObj "number" obj
        <*> valFromObj "uploader" obj
        <*> (valFromObj "approvals" obj <|> pure [])
  showJSON _ = error "unimplemented"

instance JSON Change where
  readJSON object = do
    obj <- readJSON object
    Change <$> (read <$> valFromObj "number" obj)
           <*> valFromObj "url" obj
           <*> valFromObj "project" obj
           <*> valFromObj "branch" obj
           <*> valFromObj "sortKey" obj
           <*> valFromObj "owner" obj
           <*> valFromObj "patchSets" obj
  showJSON _ = error "unimplemented"

instance JSON Stats where
  readJSON object = do
    obj <- readJSON object
    Stats <$> valFromObj "rowCount" obj
  showJSON _ = error "unimplemented"


-- Handle top-level json entries.  These alternate between change
-- descriptions (w/ a "project" entry) and a stats element that is
-- always at the end and pretty much contains only the row count for
-- our query.
instance JSON GerritJsonRes where
  readJSON object = do
    (GerritJsonC <$> readJSON object) <|> (GerritJsonStats <$> readJSON object)
  showJSON _ = error "unimplemented"

decodeGerritJson :: String -> IO (Either String GerritJsonRes)
decodeGerritJson = return . resultToEither . decode
