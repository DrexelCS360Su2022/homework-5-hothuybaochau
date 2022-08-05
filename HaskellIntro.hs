{-# OPTIONS_GHC -fwarn-tabs #-}

module HaskellIntro where

import Set
import Control.Concurrent (yield)
import Distribution.Simple.Setup (trueArg)
import Distribution.Simple.Utils (xargs)
import Text.Parsec.Token (GenTokenParser(squares))
import System.Win32 (COORD(x))
import Set (isEmpty, mapSet, singleton)
import Data.Bool (otherwise)

-- Load this file into GHCi (say, with `ghci HaskellIntro.hs`) and type
-- `isThisWorking` at the prompt. GHCi will tell you whether it's working!

isThisWorking :: String
isThisWorking = "Yes"

--
-- Problem 1
--

lastDigit :: Integer -> Integer
lastDigit n = n `mod` 10

dropLastDigit :: Integer -> Integer
dropLastDigit n = (n - (lastDigit n)) `div` 10  

toDigits :: Integer -> [Integer]
toDigits n  | n > 0     = toDigits (dropLastDigit n) ++ [lastDigit n]
            | otherwise = []

evenLength::[Integer] -> Bool
evenLength n    | (length n) `mod` 2 == 0   = True
                | otherwise             = False

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:xs) | evenLength (x:xs) = (x * 2) : doubleEveryOther xs
                        | otherwise         = x : doubleEveryOther xs       

sumOfElement :: Integer -> Integer
sumOfElement 0 = 0
sumOfElement n = lastDigit n + sumOfElement(dropLastDigit n)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sumOfElement x + sumDigits xs

validate :: Integer -> Bool
validate n  | ((sumDigits(doubleEveryOther(toDigits(dropLastDigit n)))) `mod` 10) == lastDigit n    = True
            | otherwise                                                                             = False

--
-- Problem 2
--

pow :: (a -> a) -> Int -> a -> a
pow f 0 = f
pow f 1 = f
pow f n = f . pow f (n - 1)



g :: Integer -> Integer
g 0 = 0
g n = n - pow g 2 (n-1)


h :: Integer -> Integer
h 0 = 0
h n = n - pow h 3 (n-1)

d :: Int -> Integer -> Integer
d i 0 = 0
d i n = n - pow (d i) i (n-1)

--
-- Problem 3
--
powerSet:: Ord a => Set a -> Set (Set a)
powerSet x  | isEmpty x = singleton empty
            | otherwise = union  (mapSet (insert (fst (split x))) (powerSet (snd (split x)))) (powerSet (snd (split x)))


