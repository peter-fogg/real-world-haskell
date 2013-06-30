import Data.List
import GHC.Exts hiding (Down)

data List a = Cons a (List a) | Nil
            deriving (Show)

toList :: List a -> [a]
toList Nil = []
toList (Cons x xs) = x:(toList xs)

data Tree a = Tree a (Tree a) (Tree a) | Empty
            deriving (Show)

-- data JavaishTree a = Tree a (Maybe (JavaishTree a)) (Maybe (JavaishTree a))
--                    deriving (Show)

numElements :: [a] -> Int
numElements [] = 0
numElements (x:xs) = succ $ numElements xs

mean :: Fractional a => [a] -> a
mean l = sum' l / (fromIntegral $ length l)
         where sum' [] = 0
               sum' (x:xs) = x + sum' xs

toPalindrome :: [a] -> [a]
toPalindrome xs = xs ++ (reverse xs)

isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome xs = if head xs == last xs then
                    isPalindrome $ mids xs
                  else False
  where mids l = tail $ take (pred $ length l) l

sortSublists :: [[a]] -> [[a]]
sortSublists = sortBy (\xs ys -> length xs `compare` length ys)

intersperse' :: a -> [[a]] -> [a]
intersperse' _ [] = []
intersperse' _ (x:[]) = x
intersperse' sep (x:xs) = x ++ [sep] ++ (intersperse' sep xs)

treeHeight :: Tree a -> Int
treeHeight Empty = 0
treeHeight (Tree _ l r) = succ $ max (treeHeight l) (treeHeight r)

data Direction = Up | Down | Straight
               deriving (Show, Eq)

angle (x1, y1) (x2, y2) = atan2 deltaY deltaX
  where deltaY = y2 - y1
        deltaX = x2 - x1

turn :: (Fractional a, Ord a) => (a, a) -> (a, a) -> (a, a) -> Direction
turn (x1, y1) (x2, y2) (x3, y3) = case direction `compare` 0 of
  EQ -> Straight
  GT -> Up
  LT -> Down
  where direction = (x2 - x1)*(y3 - y1) - (y2 - y1)*(x3 - x1)

directions :: (Fractional a, Ord a) => [(a, a)] -> [Direction]
directions [x, y, z] = [turn x y z]
directions (x:y:z:xs) = (turn x y z):(directions (y:z:xs))

testPoints :: (Fractional a, Ord a) => [(a, a)]
testPoints = [(0, 0), (1, 0), (1, 1), (0, 0.5), (-1, 1)]

grahamScan :: RealFloat a => [(a, a)] -> [(a, a)]
grahamScan points = foldr (\ (point, dir) acc -> if dir == Down then acc else point:acc) [] (zip rest turns)
  where p = minimumBy (\ (_, y1) (_, y2) -> y1 `compare` y2) points
        rest = (sortWith (angle p) $ delete p points) ++ [p]
        turns = directions $ p:rest