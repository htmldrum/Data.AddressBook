module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Math (sqrt, pi)

--  Number -> Number -> Number
diagonal :: Number -> Number -> Number
diagonal w h = sqrt(w * w + h * h)
--

circleArea :: Number -> Number
circleArea r = r * r * pi

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = logShow (diagonal 3.0 4.0)
