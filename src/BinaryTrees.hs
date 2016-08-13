module BinaryTrees where

data Tree a = Empty | Branch a (Tree a) (Tree a)
    deriving (Show, Eq)

tree1 = Branch 'a' (Branch 'b' (Branch 'd' Empty Empty)
                               (Branch 'e' Empty Empty))
                   (Branch 'c' Empty
                               (Branch 'f' (Branch 'g' Empty Empty)
                                           Empty))

leaf :: a -> Tree a
leaf a = Branch a Empty Empty

tree1' = Branch 'a' (Branch 'b' (leaf 'd')
                                (leaf 'e'))
                    (Branch 'c' Empty
                                (Branch 'f' (leaf 'g')
                                            Empty))

tree64 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'h'
                                        (Branch 'g'
                                                (Branch 'e' Empty Empty)
                                                Empty
                                        )
                                        Empty
                                )
                        )
                        (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 's'
                                        (Branch 'q' Empty Empty)
                                        Empty
                                )
                        )
                        Empty
                )

cbalTree :: Int -> [Tree Char]
cbalTree 0 = []
cbalTree 1 = [leaf 'X']
cbalTree 2 = [Branch 'X' (leaf 'X') Empty, Branch 'X' Empty (leaf 'X') ]
cbalTree n | n `mod` 2 == 0 = [Branch 'X' a b | a <- cbalTree (n `div` 2), b <- cbalTree((n`div`2) - 1)]
                ++ [Branch 'X' b a | a <- cbalTree (n `div` 2), b <- cbalTree((n`div`2) - 1)]
           | otherwise = [Branch 'X' a b | a <- cbalTree (n `div` 2), b <- cbalTree((n`div`2))]

mirror :: Tree a -> Tree a
mirror Empty = Empty
mirror (Branch x left right)= Branch x (mirror right) (mirror left)

(=~) :: Tree a -> Tree a -> Bool
(=~) Empty Empty = True
(=~) (Branch _ la ra) (Branch _ lb rb) = la =~ lb && ra =~ rb
(=~)  _ _ = False

symmetric :: Eq a => Tree a -> Bool
symmetric Empty = True
symmetric (Branch a left right) = (mirror left) =~ right

add :: Ord a => a -> Tree a -> Tree a
add a Empty = leaf a
add a (Branch b left right) | a < b = Branch b (add a left) right
                            | otherwise = Branch b left (add a right)
cons :: Ord a => [a] -> Tree a -> Tree a
cons [] tree = tree
cons (x:xs) tree = cons xs (add x tree)

construct :: Ord a => [a] -> Tree a
construct xs = cons xs Empty

symCbalTrees :: Int -> [Tree Char]
symCbalTrees x = filter symmetric (cbalTree x)

countLeaves :: Tree a -> Int
countLeaves Empty = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ l r) = countLeaves l + countLeaves r


leaves :: Tree a -> [a]
leaves Empty = []
leaves (Branch x Empty Empty) = [x]
leaves (Branch _ l r) = leaves l ++ leaves r

internals :: Tree a -> [a]
internals  Empty = []
internals (Branch x Empty Empty) = []
internals (Branch x l r) = x : (internals l ++ internals r)

atLevel :: Tree a -> Int -> [a]
atLevel Empty _ = []
atLevel (Branch x left right) level | level == 1 = [x]
                                    | level > 1 = atLevel left (level - 1) ++ atLevel right (level - 1)

numElements :: Tree a -> Int
numElements tree = length (leaves tree) + length (internals tree)


layout :: Tree a -> Tree (a, (Int, Int))
layout Empty = Empty
layout (Branch a left right) =
    Branch (a, (pos, depth))
    (layout' left (1, pos-1) (depth + 1))
    (layout' right (pos + 1, totalElems) (depth + 1))
    where tree = (Branch a left right)
          pos = numElements left + 1
          depth = 1
          totalElems = numElements tree
          layout' Empty _ _ = Empty
          layout' (Branch x left' right') (start', end') depth' =
            Branch (x, (  start' + numElements left', depth'))
                (layout' left' (start', start' + numElements left') (depth' + 1))
                (layout' right' (start' + numElements left' + 1, end') (depth' + 1))
