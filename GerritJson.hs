
module GerritJson (
    GerritJsonRes(..)
  , Stats(..)
  , Change(..)
  , Approval(..)
  , Rev(..)
  , Author(..)
  , ReviewType(..)
  , decodeGerritJson
  ) where

import Data.Time (UTCTime)
import Data.Time.Format (readTime)
import Data.Maybe (fromMaybe)
import System.Locale (defaultTimeLocale)
import Text.JSON

data GerritJsonRes = GerritJsonC Change | GerritJsonStats Stats deriving (Eq, Show)

data Stats = Stats Int deriving (Eq, Show)

data Change = Change {
  changeId :: Int,
  url :: String,
  project :: String,
  branch :: String,
  sortKey :: String,
  owner :: Author,
  revs :: [Rev]
  } deriving (Eq, Show)

instance Ord Change where
  (<=) a b = changeId a <= changeId b

data Author = Author { name :: String, email :: Maybe String } deriving (Eq, Ord, Show)

data ReviewType = Crvw | Subm | Vrif deriving (Eq, Show)

data Approval = Approval {
  approvalType :: ReviewType,
  value :: Int,
  grantedOn :: UTCTime,
  approver :: Author
  } deriving (Eq, Show)

data Rev = Rev {
  revId :: Int,
  uploader :: Author,
  approvals :: [Approval]} deriving (Eq, Show)

jslookupConv :: JSON a => (a -> b) -> String -> JSObject JSValue -> Result b
jslookupConv f a o =
  maybe (fail $ "No such element: " ++ a) (convResult . readJSON) $ lookup a $ fromJSObject o
  where convResult (Ok j) = Ok $ f j
        convResult (Error e) = Error e

jslookupConvOrMissing :: JSON a => (a -> b) -> (Maybe b -> c) -> String -> JSObject JSValue -> Result c
jslookupConvOrMissing f handleMissing a o =
  let a' = lookup a $ fromJSObject o
  in case a' of
    Just v ->
      (convResult . readJSON) v
      where convResult (Ok j) = Ok (handleMissing $ Just $ f j)
            convResult (Error e) = Error e
    Nothing ->
      Ok (handleMissing Nothing)

jslookup :: JSON a => String -> JSObject JSValue -> Result a
jslookup = jslookupConv id

jslookupOrEmpty :: String -> JSObject JSValue -> Result (Maybe String)
jslookupOrEmpty = jslookupConvOrEmpty id

jslookupConvOrEmpty :: JSON a => (a -> b) -> String -> JSObject JSValue -> Result (Maybe b)
jslookupConvOrEmpty f = jslookupConvOrMissing f id

instance JSON Author where
  readJSON (JSObject obj) =
    do
      n <- jslookup "name" obj
      e <- jslookupOrEmpty "email" obj
      return Author { name = n, email = e }
  readJSON _ = error "unimplemented"
  showJSON _ = error "unimplemented"

toApproval :: String -> ReviewType
toApproval "CRVW" = Crvw
toApproval "SUBM" = Subm
toApproval "VRIF" = Vrif
toApproval _ = error "unknown review type"

fromUnixTime :: Int -> UTCTime
fromUnixTime = readTime defaultTimeLocale "%s" . show

instance JSON Approval where
  readJSON (JSObject obj) =
    do
      at <- jslookupConv toApproval "type"  obj
      value_ <- jslookupConv read "value" obj
      approver_ <- jslookup "by" obj
      grantedOn_ <- jslookupConv fromUnixTime "grantedOn" obj
      return Approval { approvalType = at,
                        value = value_,
                        grantedOn = grantedOn_,
                        approver = approver_}
  readJSON _ = error "unimplemented"
  showJSON _ = error "unimplemented"

instance JSON Rev where
  readJSON (JSObject obj) =
    do
      revId_ <- jslookupConv read "number" obj
      uploader_ <- jslookup "uploader" obj
      approvals_ <- jslookupConvOrMissing id (fromMaybe []) "approvals" obj
      return Rev {
        revId = revId_,
        uploader = uploader_,
        approvals = approvals_
        }
  readJSON _ = error "unimplemented"
  showJSON _ = error "unimplemented"

instance JSON Change where
  readJSON (JSObject obj) =
    do
      changeId_ <- jslookupConv read "number" obj
      project_ <- jslookup "project" obj
      branch_ <- jslookup "branch" obj
      sortKey_ <- jslookup "sortKey" obj
      owner_ <- jslookup "owner" obj
      revs_ <- jslookup "patchSets" obj
      url_ <- jslookup "url" obj
      return Change {
        changeId = changeId_, project = project_, branch = branch_, sortKey = sortKey_,
        owner = owner_,
        revs = revs_, url = url_
        }
  readJSON _ = error "unimplemented"
  showJSON _ = error "unimplemented"

instance JSON Stats where
  readJSON (JSObject obj) =
    do
      f <- jslookup "rowCount" obj
      return $ Stats f
  readJSON _ = error "unimplemented"

  showJSON _ = error "unimplemented"


-- Handle top-level json entries.  These alternate between change
-- descriptions (w/ a "project" entry) and a stats element that is
-- always at the end and pretty much contains only the row count for
-- our query.
instance JSON GerritJsonRes where
  readJSON o@(JSObject obj) =
    if any (("project" ==) . fst) (fromJSObject obj) then
      fmap GerritJsonC (readJSON o) else
      fmap GerritJsonStats (readJSON o)

  readJSON _ = error "unimplemented"
  showJSON _ = error "unimplemented"

decodeGerritJson :: String -> IO (Either String GerritJsonRes)
decodeGerritJson text =
  do
    let j = decode text
    return (resultToEither j)
