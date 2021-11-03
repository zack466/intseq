module Intseq where

import Data.List

-- Seuence Manipulation

-- The sum of each two elements in a list
sum2 :: [Integer] -> [Integer]
sum2 (x:y:z) = (x + y) : sum2 (y:z)
sum2 _ = []

zipSum = zipWith (+)

-- Natural numbers
natural :: [Integer]
natural = [1..]

natural0 :: [Integer]
natural0 = [0..]

-- triangular numbers - A000217
triangular :: [Integer]
triangular = [n * (n + 1) `div` 2 | n <- natural0]

-- square numbers - A000290
square :: [Integer]
square = [n * n | n <- natural0]

-- pentagonal numbers - A000326
pentagonal :: [Integer]
pentagonal = [n * (3 * n - 1) `div` 2 | n <- natural0]

-- Fibonacci sequence - A000045
fibonacci :: [Integer]
fibonacci = 0 : 1 : zipSum fibonacci (tail fibonacci)

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = product [1..n]

-- lower factorial
factorialL :: Integer -> Integer -> Integer
factorialL n k = product [n-k+1 .. n]

-- upper factorial
factorialU :: Integer -> Integer -> Integer
factorialU n k = product [n..n+k-1]

-- combinations
choose :: Integer -> Integer -> Integer
choose n k = factorialL n k `div` factorial k

-- permutations
permute :: Integer -> Integer -> Integer
permute = factorialL

-- the nth row of pascal's triangle
pascalRow :: Integer -> [Integer]
pascalRow n = map (choose n) [0..n]

-- Pascal's triangle, iterated row by row - A007318
pascal :: [Integer]
pascal = natural >>= pascalRow

-- Catalan numbers - A000108
catalan :: [Integer]
catalan = [(2*n) `choose` n - (2*n) `choose` (n+1) | n <- natural0]

-- All powers of n
powers :: Integer -> [Integer]
powers n = iterate (*n) 1

-- All prime numbers - A000040
primes :: [Integer]
primes = sieve [2..]
    where
        -- Sieve of Erastothenes
        sieve :: [Integer] -> [Integer]
        sieve seq = head seq : filter (\x -> x `mod` head seq /= 0) (sieve $ tail seq)

-- All primes less than or equal to n
primesTo :: Integer -> [Integer]
primesTo n = takeWhile (<=n) primes

-- merges two sorted lists
mergeSorted :: [Integer] -> [Integer] -> [Integer]
mergeSorted a@(x:xs) b@(y:ys)
    | x < y = x : mergeSorted xs b
    | otherwise = y : mergeSorted a ys
mergeSorted _ y = y

-- n-smooth numbers
-- see https://stackoverflow.com/a/12482407
smooth :: Integer -> [Integer]
smooth n = mergeFactors $ primesTo n
    where
        -- mergeSorted, but ignores the first element of the first list provided
        merge1 (x:xs) y = x : mergeSorted xs y
        merge1 _ y = y

        -- Returns a sorted list of integers of the form p^i * q^j * r*k ...
        -- given primes [p, q, r..] and integers i, j, k, ... >= 0
        mergeFactors :: [Integer] -> [Integer]
        mergeFactors [] = [1]
        mergeFactors (p:ps) = foldl combine (iterate (*p) 1) ps
            where
                -- merges a sorted stream with itself multiplied by some power of n
                combine stream n = merge1 stream (map (* n) (combine stream n))

-- The Hamming Sequence, aka 5-smooth numbers - A051037
hamming :: [Integer]
hamming = smooth 5

