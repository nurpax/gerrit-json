
import Data.Maybe (mapMaybe)
import System.Process (readProcess)
import System.FilePath (joinPath, takeBaseName)
import System.Environment (getArgs)
import System.Directory (getDirectoryContents, doesFileExist)
import Text.Printf (printf)
import Control.Monad (filterM)
import Data.Function (on)
import Data.List (sortBy)

import GerritJson

pickChanges :: [Either String GerritJsonRes] -> [Change]
pickChanges = mapMaybe pickChange where
  pickChange (Right (GerritJsonC c)) = Just c
  pickChange (Right (GerritJsonStats _)) = Nothing
  pickChange (Left _) = Nothing

-- Must be less than Gerrit's maximum response size (IIRC 500 rows)
gerritQueryLimit = 100

readJson :: (String -> IO String) -> Maybe String -> IO (Maybe String, [Change])
readJson inputProc sortkey =
  fmap (pick . pickChanges) (inputProc queryStr >>= decode) where
    sortk = maybe "" ("resume_sortkey:" ++) sortkey
    limit = "limit:"++show gerritQueryLimit
    queryStr = "--format=JSON --patch-sets --all-approvals "++ limit ++ " " ++ sortk ++ " status:merged"
    decode = mapM decodeGerritJson . lines
    pick [] = (Nothing, [])
    pick c = (Just $ sortKey $ last c, c)

-- Query Gerrit via SSH.  The complication here is that Gerrit caps
-- its output to max 500 rows at a time.  Thus we need to use
-- resume_sortkey and do multiple queries against Gerrit to get full
-- results.
readJsonsSsh :: String -> Int -> IO [Change]
readJsonsSsh serverName maxChanges = readJsons' Nothing [] where
  sshInput q = readProcess "ssh" ["-p", "29418", serverName, "gerrit", "query", q] []
  readJsons' prevKey changesAcc =
    if length changesAcc >= maxChanges then return changesAcc
    else do
      putStr (printf "reading elems (%d)..\n" $ length changesAcc)
      (prevKey', changes) <- readJson sshInput prevKey
      if length changes < gerritQueryLimit then
        return (changesAcc++changes) else
        readJsons' prevKey' (changesAcc++changes)

getFilesInDir :: String -> IO [FilePath]
getFilesInDir path =
  do
    allEntries <- getDirectoryContents path
    fullPaths <- mapM (\x -> return $ joinPath [path, x]) allEntries
    files <- filterM doesFileExist fullPaths
    return (sortBy (compare `on` takeBaseName) files)

-- Mock version of "SSH input" (readJsonSsh above) that reads JSON
-- from files in dir tests/1.  This is handy for testing -- no need to
-- bombard a production server with queries.
readJsonsMock :: IO [Change]
readJsonsMock =
  do
    mockFiles <- getFilesInDir "tests/1"
    jsons <- mapM (\x -> readJson (\_ -> readFile x) Nothing) mockFiles
    return $ concatMap snd jsons

-- Query Gerrit via SSH & JSON and print the parsed result
queryGerrit :: String -> IO ()
queryGerrit serverName =
  readJsonsSsh serverName 500 >>= mapM_ print

main :: IO ()
main =
  do
    args <- getArgs
    if length args < 1 then
      putStrLn "Usage: prog <gerrit server name>"
      else queryGerrit (args !! 0)
