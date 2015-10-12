module Main where

import Debug.Trace

-- 0
fac :: Integer -> Integer
fac 0 = 1
fac x = x * fac (x-1)

-- 1. Get last element in list
myLast :: [a] -> a
myLast (x : xs) = if length xs == 0 then x else myLast xs

-- 1 nice
myLastNice :: [a] -> a
myLastNice [] = error "No end for empty lists!"
myLastNice [x] = x
myLastNice (_:xs) = myLastNice xs

-- 2. Get element before the last one in the list
myButLast :: [a] -> a
myButLast [] = error "No end for empty lists!"
myButLast (x:(_:[])) = x
myButLast (x:xs) = myButLast xs

-- 3. Get n-th element in the list
elementAt :: [a] -> Integer -> a
elementAt [] i = error "Index out of bounds"
elementAt (x:xs) 1 = x
elementAt (x:xs) i = elementAt xs (i-1)

-- 4. Get list length
myLength :: [a] -> Integer
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

-- 5. Reverse the list
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

-- 5. Reverse using Prelude
reversePrelude :: [a] -> [a]
reversePrelude xs = foldl (flip (:)) [] xs

-- 6. Is palindrome
palindromeCmp :: Eq a => [a] -> [a] -> Bool
palindromeCmp [x] [y] = x == y
palindromeCmp (x:xs) (y:ys) = if x == y 
								then palindromeCmp xs ys 
								else False

isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome xs = palindromeCmp xs $ reversePrelude xs

---------------------------------------------------------------------------------------------------------------
-- Custom tests
---------------------------------------------------------------------------------------------------------------
myadd :: Integer -> Integer -> Integer
myadd a b = a + b

-- lamda example
myinc :: Integer -> Integer
myinc = \x -> x + 1

mymap :: (a->b) -> [a] -> [b]
mymap f [] = []
mymap f (x:xs) = f x : mymap f xs

-- infinite list example
fib = 1 : 1 : [ a + b | (a,b) <- zip fib (tail fib) ]

dublicate :: [a] -> [a]
dublicate [] = []
dublicate (x:xs) = x : x : dublicate xs

mysign :: Integer -> Integer
mysign x 	| x > 0 	= 1
		| x == 0 	= 0
		| x < 0 	= -1

-- let example 
letTest :: Integer -> Integer -> Integer -> Integer -> Integer
letTest a b c d = let 	y = a * b 
			f x = (x + y) * y
			in f c + f d


main = do 
	print $ isPalindrome [1,2,3,4,3,2,1];
	print $ letTest 1 2 3 4;
