import qualified Data.List as List
--Chapter 4

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