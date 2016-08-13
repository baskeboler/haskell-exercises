module Exercise1to10 where

-- Problem 1
myLast :: [a] -> a
myLast = head . reverse

-- Problem 2
myButLast :: [a] -> a
myButLast = head . tail . reverse

-- Problem 3
elementAt :: [a] -> Int -> a
elementAt xs i | i < 1 = error "index must be greater than 1"
               |i == 1 = head xs
               | otherwise = elementAt (tail xs) (i-1)

-- Problem 4
myLength :: [a] -> Int
myLength  = sum . map (const 1)

-- Problem 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse xs = myLast xs :  myReverse (init xs)

-- Problem 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs | null xs = True
                | null $ tail xs = True
                | head xs == myLast xs = isPalindrome $ tail $ init xs
                | otherwise = False

-- Problem 7
data NestedList a = Elem  a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem a) = a:[]
flatten (List []) = []
flatten (List (x:xs))= flatten x ++ flatten (List xs)

--Problem 8
compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:xs) | x == y = compress (x:xs)
                  | otherwise = x : compress (y:xs)

-- Problem 9
pack :: Eq a => [a] -> [[a]]
pack xs = myReverse $ foldr (pack') [] (map (:[]) xs)
    where pack' xs res | (null res) = xs:[]
                       | head (myLast res) == (head xs) = init res ++ ( myLast res ++ xs):[]
                       | otherwise = res ++ (xs:[])

-- Problem 10
encode :: Eq a => [a] -> [(Int, a)]
encode xs = map mapper' (pack xs)
    where mapper' xs = (myLength xs, head xs)