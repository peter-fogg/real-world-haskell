import System.Environment (getArgs)
import Data.Maybe (fromMaybe)
import Data.Char (digitToInt, intToDigit)
import Control.Monad (foldM)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast [x] = Just x
safeLast (x:xs) = Just $ case safeLast xs of
  Nothing -> x
  Just xs' -> xs'

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit (x:xs) = Just $ case safeInit xs of
  Nothing -> []
  Just xs' -> x:xs'

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith pred (x:xs) = if pred x then []:(splitWith pred xs)
                        else case splitWith pred xs of
                          [] -> [[x]]
                          (y:ys) -> (x:y):ys

firstWords :: String -> [String]
firstWords = map (head . words) . lines

tails :: [[a]] -> Maybe [[a]]
--tails xs = Just xs
tails xs = if (any null xs') then Nothing
           else Just xs'
  where xs' = map tail xs

zipList :: [[a]] -> [a]
zipList [] = []
zipList xs = (map head xs) ++ (zipList $ fromMaybe [] (tails xs))

-- This isn't actually the transpose called for in the book; I
-- implemented a similar but different function. Oh well.
transpose :: String -> String
transpose s = zipList $ lines s

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where
    mainWith function = do
      args <- getArgs
      case args of
        [input,output] -> interactWith function input output
        _ -> putStrLn "error: exactly two arguments needed"
    myFunction = unlines . firstWords

-- asInt_fold :: String -> Int
-- asInt_fold s = foldl step 0 s
--   where step acc c = (10 * acc) + (digitToInt c)

asInt_fold :: String -> Int
asInt_fold [] = error "empty string"
asInt_fold ('-':s) = negate $ asInt_fold s
asInt_fold s = foldl step 0 s
  where step acc c = if c `elem` digits then (10 * acc) + (digitToInt c)
                     else error $ c:" not a digit"
        digits = map intToDigit [0..9]

type ErrorMessage = String

asInt_either :: String -> Either ErrorMessage Int
asInt_either [] = Left "empty string"
asInt_either ('-':s) = fmap negate $ asInt_either s
asInt_either s = foldM step 0 s -- using foldM: color me satisfied
  where step acc c = if c `elem` digits then Right $ (10 * acc) + (digitToInt c)
                     else Left $ c:" not a digit"
        digits = map intToDigit [0..9]

concat' :: [[a]] -> [a]
concat' = foldr step []
  where step xs acc = xs ++ acc

-- takeWhile' :: (a -> Bool) -> [a] -> [a]
-- takeWhile' _ [] = []
-- takeWhile' pred (x:xs) = if pred x then x:(takeWhile' pred xs)
--                          else []

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' pred = foldr step []
  where step x acc = if pred x then x:acc
                          else []

groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' pred = foldr step []
  where step x [] = [[x]]
        step x acc@(group:rest) = if pred x (head group)
                                  then (x:group):rest
                                  else [x]:acc

-- any: foldable
-- cycle: not foldable
-- words: foldable
-- unlines: foldable