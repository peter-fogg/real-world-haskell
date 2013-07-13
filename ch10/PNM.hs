import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Char (isSpace, isDigit, chr)
import Data.Int (Int64)
import Data.Word (Word8)
import Data.Bits (bit)
import Control.Monad (guard, liftM)
import Control.Applicative((<$>))

data GreyMap = GreyMap {
  greyWidth :: Int
  , greyHeight :: Int
  , greyMax :: Int
  , greyData :: L.ByteString
  } deriving (Eq)

instance Show GreyMap where
  show (GreyMap w h m _) = "GreyMap " ++ show w ++ "x" ++ show h ++ " " ++ show m

-- These can still likely be improved (I'll probably see it soon), but
-- the original code was godawful with do notation

matchHeader :: L.ByteString -> L.ByteString -> Maybe L.ByteString
matchHeader prefix str = do
  guard $ prefix `L8.isPrefixOf` str
  return $ L8.dropWhile isSpace (L.drop (L8.length prefix) str)

getNat :: L.ByteString -> Maybe (Int, L.ByteString)
getNat s = do
  (num, rest) <- L8.readInt s
  guard $ num > 0
  return $ (fromIntegral num, rest)

getBytes :: Int -> L.ByteString -> Maybe (L.ByteString, L.ByteString)
getBytes n str = do
  let count = fromIntegral n
      both@(prefix, _) = L.splitAt count str
  guard $ L.length prefix > count
  return both

parseP5 :: L.ByteString -> Maybe (GreyMap, L.ByteString)
parseP5 s = do
  s1 <- matchHeader (L8.pack "P5") s
  (width, s2) <- getNat s1
  (height, s3) <- getNat (L8.dropWhile isSpace s2)
  (maxGrey, s4) <- getNat (L8.dropWhile isSpace s3)
  guard $ maxGrey <= 255
  (_, s5) <- getBytes 1 s4
  (bitmap, s6) <- getBytes (width * height) s5
  return (GreyMap width height maxGrey bitmap, s6)

-- I also refuse to write out the (>>?) pseudo-bind version

data ParseState = ParseState {
  string :: L.ByteString
  , offset :: Int64
  }

newtype Parse a = Parse {
  runParse ::  ParseState -> Either String (a, ParseState)
  }

identity :: a -> Parse a
identity a = Parse (\ s -> Right (a, s))

parse :: Parse a -> L.ByteString -> Either String a
parse parser initstate = fmap fst $ runParse parser (ParseState initstate 0)

modifyOffset :: ParseState -> Int64 -> ParseState
modifyOffset initstate newOffset = initstate { offset = newOffset}

getState :: Parse ParseState
getState = Parse (\ s -> Right (s, s))

putState :: ParseState -> Parse ()
putState s = Parse (\ _ -> Right ((), s))

bail :: String -> Parse a
bail err = Parse $ \s -> Left $ "byte offset " ++ show (offset s) ++ ": " ++ err

-- Dammit dammit dammit I'm doing it monadically
instance Monad Parse where
  return = identity
  firstParser >>= secondParser = Parse $ \ state -> do
    (result, newState) <- runParse firstParser state
    runParse (secondParser result) newState
  fail = bail

parseByte :: Parse Word8
parseByte = do
  getState >>= \initState ->
    case L.uncons (string initState) of
      Nothing -> bail "no more input"
      Just (byte, remainder) ->
        putState newState >>= \ _ ->
        return byte
        where newState = initState { string = remainder,
                                     offset = newOffset }
              newOffset = offset initState + 1

instance Functor Parse where
  fmap f parser = do
    result <- parser
    return $ f result

w2c :: Word8 -> Char
w2c = chr . fromIntegral

parseChar :: Parse Char
parseChar = w2c <$> parseByte

peekByte :: Parse (Maybe Word8)
peekByte = (fmap fst . L.uncons . string) <$> getState

peekChar :: Parse (Maybe Char)
peekChar = fmap w2c <$> peekByte

parseWhile :: (Word8 -> Bool) -> Parse [Word8]
parseWhile p = (fmap p <$> peekByte) >>= \mp ->
  if mp == Just True
  then parseByte >>= \b ->
    (b:) <$> parseWhile p
  else return []

parseWhileWith :: (Word8 -> a) -> (a -> Bool) -> Parse [a]
parseWhileWith f p = fmap f <$> parseWhile (p . f)

parseNat :: Parse Int
parseNat = parseWhileWith w2c isDigit >>= \ digits ->
  if null digits
  then fail "no more input"
  else let n = read digits
       in if n < 0
          then bail "integer overflow"
          else return n

skipSpaces :: Parse ()
skipSpaces = parseWhileWith w2c isDigit >> return ()

-- This is basically guard, but I'm not sure how to work out the
-- MonadPlus instance/getting the string in there
assert :: Bool -> String -> Parse ()
assert True _ = return ()
assert False err = fail err

parseBytes :: Int -> Int -> Parse L.ByteString
parseBytes n numBytes = do
  st <- getState
  let n' = fromIntegral n
      numBytes' = fromIntegral numBytes
      (h, t) = L.splitAt n' (string st)
      st' = st { offset = offset st + (numBytes' * L.length h), string = t}
  putState st'
  assert (L.length h == n') "end of input"
  return h

notWhite :: Char -> Bool
notWhite = (`notElem` " \r\t\n")

parseRawPGM = do
  width <- parseNat; skipSpaces
  height <- parseNat; skipSpaces
  maxGrey <- parseNat; skipSpaces
  parseByte
  bitmap <- parseBytes (width * height) (if maxGrey > 255 then 2 else 1)
  return $ GreyMap width height maxGrey bitmap

repeatedly :: Int -> Parse a -> Parse [a]
repeatedly 0 _ = return []
repeatedly n parser = do
  next <- parser
  (next:) <$> repeatedly (n - 1) parser

parseNatSkip :: Parse Int
parseNatSkip = do
  n <- parseNat
  skipSpaces
  return n

toByteString :: [Int] -> L.ByteString
toByteString nums = L.pack $ map bit nums

parsePlain :: Int -> Parse L.ByteString
parsePlain n = do
  st <- getState
  let n' = fromIntegral n
  nums <- repeatedly n' parseNatSkip
  return $ toByteString nums

parsePlainPGM = do
  width <- parseNat; skipSpaces
  height <- parseNat; skipSpaces
  maxGrey <- parseNat; skipSpaces
  parseByte
  bitMap <- parsePlain (width * height)
  return $ GreyMap width height maxGrey bitMap

parsePGM = do
  header <- parseWhileWith w2c notWhite
  skipSpaces
  case header of
    "P5"      -> parseRawPGM
    "P2"      -> parsePlainPGM
    otherwise -> fail "invalid header"
