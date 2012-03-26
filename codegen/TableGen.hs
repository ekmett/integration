{-# LANGUAGE ParallelListComp #-}

import Data.Number.CReal
import Numeric.AD
import Text.Groom

import Data.List (foldl')

data DD = DD {-# UNPACK #-} !Double {-# UNPACK #-} !Double
  deriving Show

f :: Floating a => a -> (a, a)
f = diff' x where x t = tanh (0.5 * pi * sinh t)

weight :: Double -> Double -> DD
weight w d = DD (dbl x) (w * dbl x')
  where (x,x') = f (realToFrac d :: CReal)
        dbl = read . showCReal 80

dz = weight 0.5 0
dd0 = map (weight 0.5) [1..3]
dd1 = map (weight 0.5) [0.5,1.5,2.5]

dd :: [[DD]]
dd = 
   [ map (\x -> weight offset (x*scale + offset)) [0..u] 
   | u <- [5,11,23,47,95,191,383,767,1535] | scale <- iterate (/2) 0.5 | offset <- iterate (/2) 0.25
   ]

groomed_dd = groom dd
