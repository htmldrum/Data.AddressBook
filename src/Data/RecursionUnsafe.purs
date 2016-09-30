import Prelude
import Data.Array.Unsafe (head, tail)

count :: forall a. (a -> Boolean) -> Array a -> Int
count _ [] = 0
count p xs = if p (unsafePartial head xs)
             then count p (unsafePartial tail xs + 1)
             else count p (unsafePartial tail xs)

countT :: forall a. (a -> Boolean) -> Array a -> Int
countT _ [] = 0
countT p xs = if p (unsafePartial head xs)
              then 1 + countT p (unsafePartial tail xs)
              else 0 + countT p (unsafePartial tail xs)
