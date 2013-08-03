{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
import System.Directory
import System.FilePath
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Int (Int64)
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as L

listDirectory :: FilePath -> IO [String]
listDirectory = liftM (filter notDots) . getDirectoryContents
  where notDots p = p /= "." && p /= ".."

countEntries :: FilePath -> WriterT [(FilePath, Int)] IO ()
countEntries path = do
  contents <- liftIO . listDirectory $ path
  tell [(path, length contents)]
  forM_ contents $ \name -> do
    let newName = path </> name
    isDir <- liftIO . doesDirectoryExist $ newName
    when isDir $ countEntries newName

foo :: Reader (M.Map String Int) Int
foo = do
  env <- ask
  let x = maybe 0 id $ M.lookup "foo" env
  let y = maybe 0 id $ M.lookup "bar" env
  return (x+y)

data AppConfig = AppConfig {
  cfgMaxDepth :: Int
  } deriving (Show)

data AppState = AppState {
      stDeepestReached :: Int
    } deriving (Show)

data AppResult = AppResult {
    filePath :: FilePath
  } deriving (Show)

-- type App = WriterT String (StateT AppState (ReaderT AppConfig IO))

type App = WriterT [AppResult] (StateT AppState (ReaderT AppConfig IO))

runApp :: App a -> Int -> IO ((a, [AppResult]), AppState)
runApp k maxDepth =
  let config = AppConfig maxDepth
      state = AppState 0
  --in runStateT (runReaderT k config) state
  in runReaderT (runStateT (runWriterT k) state) config

-- --runApp :: App a -> Int -> IO ((a, AppState), b)
-- -- dude I don't even
-- runApp k maxDepth =
--     let config = AppConfig maxDepth
--         state = AppState 0
--         results = ""
--     in runWriterT (runReaderT (runStateT k state) config) results

constrainedCount :: Int -> FilePath -> App ()
constrainedCount curDepth path = do
  contents <- liftIO . listDirectory $ path
  cfg <- ask
  tell $ [AppResult path]
  forM_ contents $ \name -> do
    let newPath = path </> name
    isDir <- liftIO $ doesDirectoryExist newPath
    when (isDir && curDepth < cfgMaxDepth cfg) $ do
      let newDepth = curDepth + 1
      st <- get
      when (stDeepestReached st < newDepth) $
        put st { stDeepestReached = newDepth }
      constrainedCount newDepth newPath

newtype MaybeT m a = MaybeT {
  runMaybeT :: m (Maybe a)
  }

bindMT :: (Monad m) => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
x `bindMT` f = MaybeT $ do
  unwrapped <- runMaybeT x
  case unwrapped of
    Nothing -> return Nothing
    Just y -> runMaybeT (f y)

returnMT :: (Monad m) => a -> MaybeT m a
returnMT a = MaybeT $ return (Just a)

failMT :: (Monad m) => t -> MaybeT m a
failMT _ = MaybeT $ return Nothing

instance (Monad m) => Monad (MaybeT m) where
  return = returnMT
  (>>=) = bindMT
  fail = failMT

instance MonadTrans MaybeT where
  lift m = MaybeT (liftM Just m)

newtype EitherT a m b = EitherT {
  runEitherT :: m (Either a b)
  }

returnET :: (Monad m) => b -> EitherT a m b
returnET x = EitherT $ return (Right x)

failET :: (Monad m) => a -> EitherT a m b
failET x = EitherT $ return (Left x)

bindET :: (Monad m) => EitherT b m a -> (a -> EitherT b m c) -> EitherT b m c
x `bindET` f = EitherT $ do
  val <- runEitherT x
  case val of
    Left err -> return $ Left err
    Right newVal -> runEitherT (f newVal)

instance (Monad m) => Monad (EitherT a m) where
  return = returnET
  (>>=) = bindET

instance MonadTrans (EitherT a) where
  lift m = EitherT (liftM Right m)

instance (MonadState s m) => MonadState s (EitherT a m) where
  get = lift get
  put k = lift (put k)

data ParseState = ParseState {
  string :: L.ByteString,
  offset :: Int64
  }

newtype Parse a = P {
  runP :: EitherT String (State ParseState) a
  } deriving (Monad)--, MonadState ParseState)

evalParse :: Parse a -> L.ByteString -> Either String a
evalParse m s = evalState (runEitherT (runP m)) (ParseState s 0)
