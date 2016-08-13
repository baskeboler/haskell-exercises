module Exercise1to10 where

myLast :: [a] -> a
myLast = head . reverse

myButLast :: [a] -> a
myButLast = head . tail . reverse

elementAt :: [a] -> Int -> a
elementAt xs i | i < 1 = error "index must be greater than 1"
               |i == 1 = head xs
               | otherwise = elementAt (tail xs) (i-1)

myLength :: [a] -> Int
myLength  = sum . map (const 1)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse xs = myLast xs :  myReverse (init xs)

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs | null xs = True
                | null $ tail xs = True
                | head xs == myLast xs = isPalindrome $ tail $ init xs
                | otherwise = False

data NestedList a = Elem  a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem a) = a:[]
flatten (List []) = []
flatten (List (x:xs))= flatten x ++ flatten (List xs)


compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:xs) | x == y = compress (x:xs)
                  | otherwise = x : compress (y:xs)

pack :: Eq a => [a] -> [[a]]
pack xs = myReverse $ foldr (pack') [] (map (:[]) xs)
    where pack' xs res | (null res) = xs:[]
                       | head (myLast res) == (head xs) = init res ++ ( myLast res ++ xs):[]
                       | otherwise = res ++ (xs:[])