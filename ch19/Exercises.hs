{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Monad.Error
import Control.Monad.State
import qualified Data.ByteString.Char8 as B

divBy :: (Monad m, Integral a) => a -> [a] -> m [a]
divBy _ [] = return []
divBy _ (0:_) = fail "division by zero"
divBy n (x:xs) = do
  rest <- divBy n xs
  return ((n `div` x) : rest)

lazyEitherDiv :: Integral a => a -> [a] -> [Either String a]
lazyEitherDiv n xs = map divFunc xs
  where divFunc 0 = Left "division by 0!"
        divFunc m = Right $ n `div` m

data ParseError = NumericOverflow
                | EndOfInput
                | Chatty String
                deriving (Eq, Ord, Show)

instance Error ParseError where
  noMsg = Chatty "ack!"
  strMsg = Chatty

newtype Parser a = P {
  runP :: ErrorT ParseError (State B.ByteString) a
  } deriving (Monad, MonadError ParseError)



liftP :: State B.ByteString a -> Parser a
liftP state = P (lift state)

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = do
  s <- liftP get
  case B.uncons s of
    Nothing                  -> throwError EndOfInput
    Just (c, s') | f c       -> liftP (put s') >> return c
                 | otherwise -> throwError (Chatty "satisfy failed")

optional :: Parser a -> Parser (Maybe a)
optional f = (liftM Just f) `catchError` \_ -> return Nothing

runParser :: Parser a -> B.ByteString -> Either ParseError (a, B.ByteString)
runParser p bs = case runState (runErrorT (runP p)) bs of
  (Left err, _) -> Left err
  (Right r, bs) -> Right (r, bs)

many :: Parser a -> Parser [a]
many p = tryMany `catchError` \_ -> return []
  where tryMany = do
          first <- p
          rest <- many p
          return $ first:rest

-- Only works for base 10; whatever
int :: Parser Int
int = do
  sign <- optional (satisfy (=='-'))
  digits <- many (satisfy (`elem` "0123456789"))
  case sign of
    -- Delegate to `read` for laziness
    Nothing -> let val = read digits
               -- This doesn't really work if we wrap around twice, meh
               in if val < 0
                  then throwError NumericOverflow
                  else return val
    Just minus -> let val = -1 * read digits
                  in if val > 0
                     then throwError NumericOverflow
                     else return val
