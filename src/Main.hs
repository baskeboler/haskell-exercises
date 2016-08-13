module Main where
import Prelude
import Data.Char
import Data.String

main :: IO()
main = print "Hello world!"
trailingString :: String -> Char -> String
trailingString [] _ = []
trailingString (x:xs) y = if x == y then xs else trailingString xs y

containsChar :: String -> Char -> Bool
containsChar [] _ = False
containsChar (x:xs) y = if x == y then True else containsChar xs y

isValidSymbol :: String -> String -> Bool

isValidSymbol _ (a:[]) = False
isValidSymbol _ (a:b:c:xs) = False
isValidSymbol name (s:y:[]) = containsChar (trailingString name s) y

insertOrdered :: String -> [String] -> [String]
insertOrdered a [] = a:[]
insertOrdered a (x:xs) | a <= x = a:x:xs
                       | a > x = x: insertOrdered a xs
insertUnique :: String -> [String] -> [String]
insertUnique  a [] = a:[]
insertUnique  a (x:xs) | a < x = a:x:xs
                       | a > x = x: insertOrdered a xs
                       | otherwise = x:xs
unique :: [String] -> [String]
unique [] = []
unique (x:[]) = x:[]
unique (x:xs)= insertUnique  x $ unique $ sort xs

sort :: [String] -> [String]
sort [] = []
sort (a:[]) = a:[]
sort (a:b:xs) = insertOrdered a $ sort $ b:xs
symbols :: String -> [String]
symbols [] = []
symbols (a:[]) = []
symbols name =  [(x:y:[]) | x <- map toLower name, y <- map toLower name, containsChar (trailingString (map toLower name) x) y]