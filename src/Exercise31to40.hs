module Exercise31to40 where
import Exercise21to30

isPrime :: Int -> Bool
isPrime a = null $
    filter isDivisor $
        range 2 (a-1)
    where isDivisor x = (a `mod` x == 0)

primes :: Int -> [Int]
primes n = [x | x <- range 2 n, isPrime x]

divisors :: Int -> [Int]
divisors n = filter (\x -> n `mod` x == 0) (range 1 n)

myGCD :: Int -> Int -> Int
myGCD a b = maximum [x | x <- divisors a, elem x (divisors b)]

coprime :: Int -> Int -> Bool
coprime a b = myGCD a b == 1