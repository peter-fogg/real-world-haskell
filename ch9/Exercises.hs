-- 1. Yes! If we call bracket outside handle, then handle will deal
-- with an exception and not call the release action, leading to a
-- resource leak. This is bad.

import Control.Monad (forM, filterM, liftM)
import System.Directory (Permissions(..), getModificationTime, getPermissions, getDirectoryContents, doesDirectoryExist)
import Data.Time.Clock (UTCTime)
import System.FilePath (takeExtension, (</>))
import Control.Exception (bracket, handle, IOException)
import System.IO (IOMode(..), hClose, hFileSize, openFile)
import GHC.Exts (sortWith)

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path
      else return [path]
  return (concat paths)

type Predicate =  FilePath      -- path to directory entry
               -> Permissions   -- permissions
               -> Maybe Integer -- file size (Nothing if not file)
               -> UTCTime     -- last modified
               -> Bool

-- Specialize the type of exception handler in order to avoid type ambiguity
handleIO :: (IOException -> IO a) -> IO a -> IO a
handleIO = handle

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = handleIO (\e -> return Nothing) $
  bracket (openFile path ReadMode) hClose $ \h -> do
    size <- hFileSize h
    return (Just size)

betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind p path = getRecursiveContents path >>= filterM check
    where check name = do
            perms <- getPermissions name
            size <- getFileSize name
            modified <- getModificationTime name
            return (p name perms size modified)

type InfoP a =  FilePath        -- path to directory entry
             -> Permissions     -- permissions
             -> Maybe Integer   -- file size (Nothing if not file)
             -> UTCTime         -- last modified
             -> a

pathP :: InfoP FilePath
pathP path _ _ _ = path

sizeP :: InfoP Integer
sizeP _ _ (Just size) _ = size
sizeP _ _ Nothing     _ = -1

equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP f k w x y z = f w x y z == k

liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP q f k w x y z = f w x y z `q` k

greaterP, lesserP :: (Ord a) => InfoP a -> a -> InfoP Bool
greaterP = liftP (>)
lesserP = liftP (<)

liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 q f g w x y z = f w x y z `q` g w x y z

andP = liftP2 (&&)
orP = liftP2 (||)

myTest path _ (Just size) _ =
    takeExtension path == ".cpp" && size > 131072
myTest _ _ _ _ = False

liftPath :: (FilePath -> a) -> InfoP a
liftPath f w _ _ _ = f w

myTest2 = (liftPath takeExtension `equalP` ".cpp") `andP`
          (sizeP `greaterP` 131072)

(==?) = equalP
(&&?) = andP
(>?) = greaterP

myTest3 = (liftPath takeExtension ==? ".cpp") &&? (sizeP >? 131072)

infix 4 ==?
infixr 3 &&?
infix 4 >?

myTest4 = liftPath takeExtension ==? ".cpp" &&? sizeP >? 131072

data Info = Info {
      infoPath :: FilePath
    , infoPerms :: Maybe Permissions
    , infoSize :: Maybe Integer
    , infoModTime :: Maybe UTCTime
    } deriving (Eq, Ord, Show)

getInfo :: FilePath -> IO Info

traverse :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverse order path = do
    names <- getUsefulContents path
    contents <- mapM getInfo (path : map (path </>) names)
    liftM concat $ forM (order contents) $ \info -> do
      if isDirectory info && infoPath info /= path
        then traverse order (infoPath info)
        else return [info]

getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
    names <- getDirectoryContents path
    return (filter (`notElem` [".", ".."]) names)

isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms

maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handleIO (\_ -> return Nothing) (Just `liftM` act)

getInfo path = do
  perms <- maybeIO (getPermissions path)
  size <- maybeIO (bracket (openFile path ReadMode) hClose hFileSize)
  modified <- maybeIO (getModificationTime path)
  return (Info path perms size modified)

-- To reverse-traverse a directory, pass a function which
-- reverse-sorts by infoPath as the order parameter.

postOrder :: [Info] -> [Info]
postOrder = sortWith (not . isDirectory)

type InfoP' a = Info -> a

pathP :: InfoP' FilePath
pathP = infoPath



sizeP :: InfoP Integer
sizeP x = case infoSize x of
  Just size -> size
  Nothing -> -1

-- equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
-- equalP f k = \x -> f x == k
