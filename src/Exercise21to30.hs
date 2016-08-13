module Exercise21to30 where
import System.Random
import Exercise11to20

insertAt :: a -> [a] -> Int -> [a]
insertAt e list 1 = e:list
insertAt e [] _ = [e]
insertAt e (x:xs) n | n < 1 = error "index must be greater than 0"
                    | otherwise = x : insertAt e xs (n-1)

range :: Int -> Int -> [Int]
range a b | a == b = [a]
          | a < b = a: range (a+1) b
          | otherwise = range b a


rndSelect :: [a] -> Int -> IO [a]
rndSelect _ 0 = return []
rndSelect [] _ = return []
rndSelect xs count = do r <- randomRIO (0, (length xs) - 1)
                        rest <- rndSelect (snd $ removeAt (r+1) xs ) (count - 1)
                        return ((xs!!r): rest)

diffSelect :: Int -> Int -> IO [Int]
diffSelect count max = rndSelect (range 1 max) count

rndPermutation :: [a] -> IO [a]
rndPermutation xs = rndSelect xs (length xs)

-- 6
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations count xs = [xs !! i : rest | i <- [0..(length xs)-1],
                                          rest <- combinations (count - 1) (drop (i+1) xs)]

--group3 :: Eq a => [a] -> [[a]]
--group3 xs =