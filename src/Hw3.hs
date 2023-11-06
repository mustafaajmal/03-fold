{- | CSE 116: All about fold.

     For this assignment, you may use the following library functions:

     length
     append (++)
     map
     foldl'
     foldr
     unzip
     zip
     reverse

  Use www.haskell.org/hoogle to learn more about the above.

  Do not change the skeleton code! The point of this assignment
  is to figure out how the functions can be written this way
  (using fold). You may only replace the `error "TBD:..."` terms.

-}

module Hw3 where

import Prelude hiding (replicate, sum)
import Data.List (foldl')

foldLeft :: (a -> b -> a) -> a -> [b] -> a
foldLeft = foldl'

--------------------------------------------------------------------------------
-- | sqSum [x1, ... , xn] should return (x1^2 + ... + xn^2)
--
-- >>> sqSum []
-- 0
--
-- >>> sqSum [1,2,3,4]
-- 30
--
-- >>> sqSum [(-1), (-2), (-3), (-4)]
-- 30

sqSum :: [Int] -> Int
sqSum xs = foldLeft f base xs
  where
   f a x = a + x * x
   base = 0
   
--------------------------------------------------------------------------------
-- | `pipe [f1,...,fn] x` should return `f1(f2(...(fn x)))`
--
-- >>> pipe [] 3
-- 3
--
-- >>> pipe [(\x -> x+x), (\x -> x + 3)] 3
-- 12
--
-- >>> pipe [(\x -> x * 4), (\x -> x + x)] 3
-- 24

pipe :: [(a -> a)] -> (a -> a)
pipe fs = foldLeft f base fs
  where
    f acc g = acc . g
    base = id

--------------------------------------------------------------------------------
-- | `sepConcat sep [s1,...,sn]` returns `s1 ++ sep ++ s2 ++ ... ++ sep ++ sn`
--
-- >>> sepConcat "---" []
-- ""
--
-- >>> sepConcat ", " ["foo", "bar", "baz"]
-- "foo, bar, baz"
--
-- >>> sepConcat "#" ["a","b","c","d","e"]
-- "a#b#c#d#e"

sepConcat :: String -> [String] -> String
sepConcat sep xs = foldLeft f base xs
  where
    f a "" = a
    f "" b = b
    f a b = a ++ sep ++ b
    base = ""

intString :: Int -> String
intString = show

--------------------------------------------------------------------------------
-- | `stringOfList pp [x1,...,xn]` uses the element-wise printer `pp` to
--   convert the element-list into a string:
--
-- >>> stringOfList intString [1, 2, 3, 4, 5, 6]
-- "[1, 2, 3, 4, 5, 6]"
--
-- >>> stringOfList (\x -> x) ["foo"]
-- "[foo]"
--
-- >>> stringOfList (stringOfList show) [[1, 2, 3], [4, 5], [6], []]
-- "[[1, 2, 3], [4, 5], [6], []]"

stringOfList :: (a -> String) -> [a] -> String
stringOfList f xs = "[" ++ sepConcat ", " (map f xs) ++ "]"

--------------------------------------------------------------------------------
-- | `clone x n` returns a `[x,x,...,x]` containing `n` copies of `x`
--
-- >>> clone 3 5
-- [3,3,3,3,3]
--
-- >>> clone "foo" 2
-- ["foo", "foo"]

clone :: a -> Int -> [a]
clone x n = foldl' (\acc _ -> acc ++ [x]) [] [1..n]

type BigInt = [Int]

--------------------------------------------------------------------------------
-- | `padZero l1 l2` returns a pair (l1', l2') which are just the input lists,
--   padded with extra `0` on the left such that the lengths of `l1'` and `l2'`
--   are equal.
--
-- >>> padZero [9,9] [1,0,0,2]
-- ([0,0,9,9],[1,0,0,2])
--
-- >>> padZero [1,0,0,2] [9,9]
-- ([1,0,0,2], [0,0,9,9])

padZero :: BigInt -> BigInt -> (BigInt, BigInt)
padZero l1 l2
  | len1 > len2 = (l1, clone 0 (len1 - len2) ++ l2)
  | otherwise = (clone 0 (len2 - len1) ++ l1, l2)
  where
    len1 = length l1
    len2 = length l2

--------------------------------------------------------------------------------
-- | `removeZero ds` strips out all leading `0` from the left-side of `ds`.
--
-- >>> removeZero [0,0,0,1,0,0,2]
-- [1,0,0,2]
--
-- >>> removeZero [9,9]
-- [9,9]
--
-- >>> removeZero [0,0,0,0]
-- []

removeZero :: BigInt -> BigInt
removeZero = dropWhile (== 0)

--------------------------------------------------------------------------------
-- | `bigAdd n1 n2` returns the `BigInt` representing the sum of `n1` and `n2`.
--
-- >>> bigAdd [9, 9] [1, 0, 0, 2]
-- [1, 1, 0, 1]
--
-- >>> bigAdd [9, 9, 9, 9] [9, 9, 9]
-- [1, 0, 9, 9, 8]

bigAdd :: BigInt -> BigInt -> BigInt
bigAdd l1 l2 = removeLeadingZeroes $ reverse (addLists (reverse l1) (reverse l2) 0)
  where
    addLists [] [] carry = if carry > 0 then [carry] else []
    addLists (x:xs) [] carry = let sum = x + carry in (sum `mod` 10) : addLists xs [] (sum `div` 10)
    addLists [] (y:ys) carry = let sum = y + carry in (sum `mod` 10) : addLists [] ys (sum `div` 10)
    addLists (x:xs) (y:ys) carry = let sum = x + y + carry in (sum `mod` 10) : addLists xs ys (sum `div` 10)

    removeLeadingZeroes = dropWhile (== 0)

--------------------------------------------------------------------------------
-- | `mulByInt i n` returns the result of multiplying
--   the int `i` with `BigInt` `n`.
--
-- >>> mulByInt 9 [9,9,9,9]
-- [8,9,9,9,1]

mulByInt :: Int -> BigInt -> BigInt
mulByInt _ [] = []
mulByInt 0 _ = [0]
mulByInt i n = removeZero (reverse (mulByInt' (reverse n) i 0))
  where
    mulByInt' [] _ c = if c == 0 then [] else [c]
    mulByInt' (x:xs) i c = let product = x * i + c
                               newDigit = product `mod` 10
                               newCarry = product `div` 10
                           in newDigit : mulByInt' xs i newCarry
    removeZero = dropWhile (== 0)

--------------------------------------------------------------------------------
-- | `bigMul n1 n2` returns the `BigInt` representing the product of `n1` and `n2`.
--
-- >>> bigMul [9,9,9,9] [9,9,9,9]
-- [9,9,9,8,0,0,0,1]
--
-- >>> bigMul [9,9,9,9,9] [9,9,9,9,9]
-- [9,9,9,9,8,0,0,0,0,1]

bigMul :: BigInt -> BigInt -> BigInt
bigMul [] _ = [0]
bigMul _ [] = [0]
bigMul l1 l2 = foldl' (\acc (product, zeros) -> bigAdd acc (product ++ zeros)) [] productsWithZeros
  where
    productsWithZeros = zipWith (\d idx -> (mulByInt d l2, replicate idx 0)) (reverse l1) [0..]
    replicate n x = foldl' (\acc _ -> acc ++ [x]) [] [1..n]

