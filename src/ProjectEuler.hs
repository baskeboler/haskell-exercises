module ProjectEuler where

lexPerms :: Ord a => [a] -> [[a]]
lexPerms [] = [[]]
lexPerms xs = [x:rest| i <- [0..(length xs) -1], let x = xs!!i, rest <- lexPerms (removeAt i xs) ]
    where
          removeAt _ [] = []
          removeAt n xs | n == 0 = tail xs
                        | otherwise = head xs : (removeAt (n-1) (tail xs))

goldenSection = (1+(sqrt 5))/2

fibDigits n = floor $ 1 + n * (logBase 10 goldenSection) - ((logBase 10 5)/2)

--1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
coinsp =   [1,2,5,10,20,50,100,200]
change [] _ = []
change _ 0 = [[]]
change coins amount = [(c, head coins):rest|
    c <- [0..amount `div`(head coins) +1],
    c*(head coins) <= amount,
    rest <- (change (tail coins) (amount-c*(head coins))),
    val ((c, head coins):rest) == amount]
    where val [] = 0
          val ((a,b):xs) = a*b+val xs