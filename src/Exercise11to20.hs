module Exercise11to20 where

import Exercise1to10(encode)

-- Problem 11
data Occurrence a = Single a | Multiple Int a deriving (Show, Eq)
single :: Eq a => a -> Occurrence a
single x = Single x

value :: Occurrence a -> a
value (Single a) = a
value (Multiple _ a) = a

occurrences :: Occurrence a -> Int
occurrences (Single _)= 1
occurrences (Multiple n _) = n

cons :: Eq a  => Occurrence a -> [Occurrence a] -> [Occurrence a]
cons x [] = [x]
cons a (x:xs) | value a == value x = (Multiple (occurrences a + occurrences x) (value a)): xs
              | otherwise = a:x:xs

encodeModified :: Eq a => [a] -> [Occurrence a]
encodeModified xs = map mapper' (encode xs)
    where mapper' (a, char) | a == 1 = Single char
                           | otherwise = Multiple a char

-- Problem 12
decodeModified :: Eq a => [Occurrence a] -> [a]
decodeModified [] = []
decodeModified (Single a: xs) = a: decodeModified xs
decodeModified (Multiple n a: xs) = replicate n a ++ decodeModified xs

-- Problem 13
encodeDirect :: Eq a => [a] -> [Occurrence a]
encodeDirect [] = []
encodeDirect xs = foldr cons [] (map single xs)

dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = [x,x] ++ dupli xs

repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = replicate n x ++ (repli xs n)

dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery (x:xs) n | n == 1 = xs
                   | otherwise = x: dropEvery xs (n-1)

split :: [a] -> Int -> ([a], [a])
split [] _ = ([], [])
split xs n = move' ([], xs) n
    where move' (as, bs) 0 = (as, bs)
          move' (as, bs) n = move' (as ++ [head bs], tail bs) (n-1)

slice :: [a] -> Int -> Int -> [a]
slice xs a b = fst $ split (snd $ split xs (a-1)) (b-a+1)

rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs 0 = xs
rotate xs n | n > 0 = let n' = n `mod` length xs
                          t = split xs n'
                      in snd t ++ fst t
            | n < 0 = let l = length xs
                          n' = (n + n*l) `mod` l
                          t = split xs n'
                      in snd t ++ fst t


removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = let tuple = split xs (n-1)
                    f = fst tuple
                    l = snd tuple
                in (head l, f ++ tail l)