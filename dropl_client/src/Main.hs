import System.Directory
import System.INotify
import System.FilePath.GlobPattern

import qualified Data.Map as M
import Data.List

import Control.Applicative
import Control.Monad
import Control.Concurrent.MVar (newMVar, readMVar, modifyMVar_)

main :: IO ()
main = do
  wdMap <- newMVar (M.empty :: M.Map FilePath WatchDescriptor)
  inotify <- initINotify
  baseDir <- getCurrentDirectory
  let evHandler = handler inotify baseDir baseDir
  wd <- addWatch inotify [Create, Modify, Move] baseDir evHandler
  putStrLn $ "Listening to " ++ (show baseDir) ++ ". Hit enter to terminate"
  getLine
  removeWatch wd

-- A file was created
handler inotify baseDir curPathOfHandler c@(Created isDir fileName)
  | isDir = do
      let path = curPathOfHandler ++ "/" ++ fileName
      let evHandler = handler inotify baseDir path
      wd <- addWatch inotify [Create, Modify, Move] path evHandler
      --modifyMVar_ wdMap $ \wdMap -> return (M.insert path wd wdMap)
      print $ "Created: " ++ path
      return ()
  | otherwise = print c

-- A file was modified
handler inotify baseDir curPathOfHandler m@(Modified isDir fileName)
  | isDir = return ()
  | otherwise = do
      let path = (++) <$> (Just curPathOfHandler) <*> fileName
      print $ (++) <$> path <*> (Just " modified.")

-- A file was moved away from the watched dir.
-- Note that inotify automatically stops watching a directory that is MovedOut
handler inotify baseDir curPathOfHandler m@(MovedOut isDir fileName cookie)
  | isDir = do
      -- We must tell the server to put this directory name in an ENV_VAR
      let pathOfMovedOutDir = curPathOfHandler ++ "/" ++ fileName
      print m
      --print $ pathOfMovedOutDir ++ " MovedOut."
      -- TODO: how to differentiate between a deleted directory and a directory
      -- that has been moved
      print $ "MovedOut: " ++ pathOfMovedOutDir
  | otherwise = print m

-- A file was moved into the watched dir.
handler inotify baseDir curPathOfHandler m@(MovedIn isDir fileName cookie)
  | isDir = do
    print m
    let pathOfMovedInDir = curPathOfHandler ++ "/" ++ fileName
    print pathOfMovedInDir
    -- Get all subdirectories (use filemanip)
    -- addWatch on subdirectories
    -- update wdMap
    return ()
  | otherwise = print m

-- The watched file was moved.
handler inotify baseDir curPathOfHandler m@(MovedSelf isDir) = print m


onModified fileName = do
      file <- getFile fileName
      oldSig <- requestSigFor fileName
      deltas <- computeDeltas oldSig file
      sendDeltas deltas
  where getFile = undefined
        requestSigFor fullPath = undefined -- HTTP GET request?
        computeDeltas = undefined -- delta sigFile file deltaFile
        sendDeltas fullPath = undefined
movedOutHandler = do
  requestPutIntoEnv
    where requestPutIntoEnv = undefined

getSubDirs :: [FilePath] -> IO [FilePath]
getSubDirs fs = do
  print "getSubDirs"
  foldl' stepFn (return []) fs
  where stepFn acc filePath = do
          print filePath
          -- Necessary for doesDirectoryExist to work properly
          setCurrentDirectory filePath

          contents <- getDirectoryContents filePath
          subDirs <- filterM isSubDir contents >>= mapM makeAbsolute
          (++) <$> ((++) <$> acc <*> (return subDirs)) <*> getSubDirs subDirs
        isSubDir ".." = return False
        isSubDir "."  = return False
        isSubDir file = doesDirectoryExist file
