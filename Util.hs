module Util (
    join,
    connect,
    split,
    trim,
    beginsWith,
    isHexa,
    hexToInt,
    toHexa,
    fixed
) where

join :: [a] -> [b] -> [(a,b)]
join (x:xs) ys = (foldr (\y a -> (x,y) : a) [] ys) ++ (join xs ys)
join [] _ = []

connect :: [a] -> [(a,a)]
connect (x:xs) = (zip (repeat x) xs) ++ (connect xs)
connect _ = []

split :: (Eq a) => [a] -> a -> [[a]]
split xs sep 
    | (elem sep xs) = (takeWhile (/= sep) xs) : 
                      (split (tail (dropWhile (/= sep) xs)) sep)
    | null xs = []
    | otherwise = [xs]

trim :: String -> String
trim str = dropWhile (== ' ') (reverse (dropWhile (== ' ') (reverse str)))

beginsWith :: (Eq a) => [a] -> [a] -> Bool
beginsWith xs ys = and $ zipWith (==) xs ys

isHexa :: String -> Bool
-- XXX isHexa "" == True
isHexa (x:xs) | x `elem` "0123456789ABCDEFabcdef" = isHexa xs
              | otherwise = False
isHexa _ = True

hexToInt :: String -> Int
hexToInt [] = 0
hexToInt str 
    | isHexa str = read ("0x" ++ str)
    | otherwise = 0 -- XXX error!

toHexa :: Int -> String
toHexa 0 = "0"
toHexa i = (toHexa $ div i 16) ++ [hexaChar $ mod i 16]
    where hexaChar i = "0123456789abcdef" !! i

fixed :: String -> Int -> String
fixed str n | (length str) > n && (head str) == '0' = fixed (tail str) (n-1)
            | (length str) < n = fixed ('0' : str) n
            | otherwise = str

