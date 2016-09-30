module Data.Recursion where

import Prelude
import Data.Int (even)
import Data.Array (null, filter, concatMap, (..))
import Data.Array.Partial (head, tail)
import Data.Int (fromNumber)
import Partial.Unsafe (unsafePartial)
import Data.Foldable(product, sum, foldr, foldl)
import Control.MonadZero (guard)

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

length :: forall a. Array a -> Int
length arr =
  if null arr
    then 0
    else 1 + length (unsafePartial tail arr)

isEvenInt :: Int -> Boolean
isEvenInt i = even i

evenPred :: Int -> Int
evenPred i = if isEvenInt i
             then 1
             else 0

countEven :: Array Int -> Int
countEven is = if null is
              then 0
              else evenPred(unsafePartial head is) + countEven(unsafePartial tail is)

squares :: Array Number -> Array Number
squares is = (\n -> n * n) <$> is

infix 8 filter as <$?>

-- map show $ isEvenInt <$?> [1,2,3, 4, 5]

noNegs :: Array Number -> Array Number
noNegs is = filter (\n -> n > 0.0) is

pairs :: Int -> Array Int
pairs n = concatMap(\i -> (1..n)) (1 .. n)

pairs':: Int -> Array (Array Int)
pairs' n = concatMap (\i -> map (\j -> [i,j]) (1 .. n)) (1 .. n)

pairs'':: Int -> Array (Array Int)
pairs'' n = concatMap (\i -> map (\j -> [i,j]) (i .. n)) (1 .. n)

factors :: Int -> Array (Array Int)
factors n = filter (\pair -> product pair == n) (pairs'' n)

factorsAsDo :: Int -> Array (Array Int)
-- factorsAsDo n = filter (\xs -> product xs == n) $ do
--   i <- 1 .. n
--   j <- i .. n
--   pure [i, j]
-- Using guard instead of filter
factorsAsDo n = do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n
  pure [i, j]

isPrime :: Int -> Boolean
isPrime i = length (factorsAsDo i) == 1

cartesianProduct :: Array Int -> Array Int -> Array (Array Int)
cartesianProduct xs ys = concatMap(\x -> map (\y -> [x, y]) ys) xs

pyTriple :: Int -> Array(Array Int)
pyTriple n = do
  a <- 1..n
  b <- 1..n
  c <- 1..n
  guard $ a*a + b*b == c*c
  pure [a, b, c]

-- p108
-- factorizations :: Int -> Array Int
-- factorizations n = do
--   a <- factors n
--   b <- sum (head a) -- typeError Array(Array Int) where Array Int expected -- how to manage different dimensions
--   guard b == n
--   pure a

factAcc :: Int -> Int -> Int
factAcc 0 acc = acc
factAcc n acc = factAcc (n-1) (acc*n)

reverse :: forall a. Array a -> Array a
reverse = foldr (\x xs -> xs <> [x]) []

reverseL :: forall a. Array a -> Array a
reverseL = foldl (\xs x -> [x] <> xs) []

allTrue :: Array Boolean -> Boolean
-- allTrue = foldl (\a x -> x && a) true -- eta conversion
allTrue = foldl (&&) true



