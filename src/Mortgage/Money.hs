module Mortgage.Money (
    Money
  ) where

import Test.QuickCheck
import Text.Printf
import Data.Ratio

data Money = Money {-# UNPACK #-} !Double

instance Show Money where
  show (Money amt) = printf "%.2f" amt

-- round to 0.01
instance Eq Money where
  (Money x) == (Money y) = round (100*x) == round (100*y)

instance Ord Money where
  compare (Money x) (Money y) = compare (round (100*x)) (round (100*y))

instance Num Money where
  (+) (Money x) (Money y) = Money (x+y)
  (-) (Money x) (Money y) = Money (x-y)
  (*) (Money x) (Money y) = Money (x*y)
  negate (Money x)        = Money (negate x)
  abs (Money x)           = Money (abs x)
  signum (Money x)
    | x == 0              = 0
    | x  > 0              = 1
    | x  < 0              = -1
  fromInteger i           = Money (fromIntegral i)

instance Fractional Money where
    (/) (Money x) (Money y) = Money (x / y)
    fromRational r = Money $ (fromIntegral . numerator $ r) / (fromIntegral . denominator $ r)

instance Real Money where
    toRational (Money x) = toRational x

instance RealFrac Money where
    properFraction (Money x) = (y, Money (x - fromIntegral y))
      where y = floor x

instance Arbitrary Money where
    arbitrary = fmap Money (choose (0, 1000000000))
