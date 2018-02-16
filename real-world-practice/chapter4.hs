import Data.Char (digitToInt, isDigit)
import qualified Data.List as List

type ErrorMessage = String
--Chapter 4

-- A --

-- 1
safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead [] = Nothing

safeTail :: [a] -> Maybe [a]
safeTail (x:xs) = Just xs
safeTail [] = Nothing

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just $ last xs

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit xs = Just $ init xs

-- 2
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith splitter input = 
	let 
		(pref, suff) = break splitter input
	in
		pref : if null suff then [] else splitWith splitter (tail suff)

-- 3
firstWords :: String -> [String]
firstWords input = foldr (\x acc -> (head . words $ x) : acc) [] (lines input)

-- 4
stringTranspose :: String -> String
stringTranspose = unlines . List.transpose . lines


-- B --

-- 1
asInt_fold :: String -> Int
asInt_fold [] = 0
asInt_fold ('-':input) = negate . asInt_fold $ input
asInt_fold input = List.foldl' consume 0 input
	where consume acc x
		| x == '.'	= error "Char.digitToInt: not a digit '.'"
		| otherwise	= acc * 10 + (digitToInt x)
		
-- 2
asInt_either :: String -> Either ErrorMessage Int
asInt_either [] = Right 0
asInt_either ('-':input) = case asInt_either $ input of
	Left msg -> Left msg
	Right x -> Right (negate x)
asInt_either input = List.foldl' consume (Right 0) input
	where
	consume (Left acc) x = Left acc
	consume (Right acc) x
		| isDigit x = Right (acc * 10 + (digitToInt x))
		| otherwise = Left ("non-digit '" ++ x : "'")
		
-- 3
concat_foldr :: [[a]] -> [a]
concat_foldr input = foldr (++) [] input