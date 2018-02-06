import Data.List

-- 8
data Tree a = Node a (Tree a) (Tree a)
			| Empty
			deriving (Show)

--9			
data Direction = TurnLeft | Straight | TurnRight deriving (Eq, Show)

-- 10
type Point = (Int, Int)
				
-- Chapter 3
-- 1,2
listSize :: [a] -> Int
listSize xs = foldl (\acc x -> acc + 1) 0 xs

-- 3
listMean :: (Fractional a) => [a] -> a
listMean xs = elemSum / (fromIntegral size)
	where
		elemSum = sum xs
		size = length xs

-- 4		
makePalindrome :: [a] -> [a]
makePalindrome xs = xs ++ reverse xs

-- 5
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs

-- 6
sortByLength :: (Ord a) => [[a]] -> [[a]]
sortByLength xs = sortBy lengthChecker xs
	where
		lengthChecker a b
			| (length a > length b)	= GT
			| (length a < length b) = LT
			| otherwise				= EQ

-- 7			
intersperser :: a -> [[a]] -> [a]
intersperser sep xs = foldr (\x acc -> x ++ [sep] ++ acc) (last xs) (init xs)

-- 8
treeHeight :: Tree a -> Int
treeHeight Empty = 0
treeHeight (Node _ x y) = max (1 + treeHeight x) (1 + treeHeight y)

-- 10
toVector :: Point -> Point -> Point
toVector (ax, ay) (bx, by) = (ax - bx, ay - by)

vectorLength :: (Floating a) => Point -> a
vectorLength (x, y) = sqrt (fromIntegral (x * x + y * y))

vectorProduct :: (Fractional a) => Point -> Point -> a
vectorProduct (ux, uy) (vx, vy) = fromIntegral (ux * vy - uy * vx)

calculateSin :: (Floating a) => Point -> Point -> Point -> a
calculateSin a b c = (vectorProduct u v) / ((vectorLength u) * (vectorLength v))
	where
		u = toVector a b
		v = toVector c b
		
getDirection :: Point -> Point -> Point -> Direction
getDirection a b c
	| calculateSin a b c > 0	= TurnRight
	| calculateSin a b c < 0	= TurnLeft
	| otherwise					= Straight

-- 11	
getDirections :: [Point] -> [Direction]
getDirections (a:b:c:[]) = [getDirection a b c]
getDirections (a:b:c:pts) = (getDirection a b c) : getDirections (b:c:pts)