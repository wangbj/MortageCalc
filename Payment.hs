module Payment (
  Payment(..),
  YearlyPayment(..),
  OneTimePayment(..),
  fromYearlyPayment,
  fromOneTimePayment
  ) where

import SimpleDTime

data YearlyPayment a = YearlyPayment a deriving (Show, Eq)
data OneTimePayment a = OneTimePayment a deriving (Show, Eq)

-- Reason: Why choose ``Maybe YearlyPayment (Int, Double)'' instead of
--            ``Maybe (Int, Double)''
-- Maybe inherits Ord, where Ord shouldn't apply to YearlyPaymnet, i.e.:
--    YearlyPayment (10, 1000.0) is not comparable to YearlyPayment (11, 1000.)

data Payment = Payment {
  monthlyPayment :: Maybe Double,
  monthlyAdditional :: Maybe Double,
  yearlyPayment :: Maybe (YearlyPayment (Int, Double)),
  oneTimePayment :: Maybe (OneTimePayment (DTime, Double))
  } deriving (Show)


instance Functor YearlyPayment where
  fmap f (YearlyPayment x) = YearlyPayment (f x)

instance Functor OneTimePayment where
  fmap f (OneTimePayment x) = OneTimePayment (f x)

instance Monad YearlyPayment where
  return = YearlyPayment
  YearlyPayment a >>= k = k a

instance Applicative YearlyPayment where
  pure = YearlyPayment
  (YearlyPayment f) <*> x = fmap f x

instance Monad OneTimePayment where
  return = OneTimePayment
  OneTimePayment a >>= k = k a

instance Applicative OneTimePayment where
  pure = OneTimePayment
  (OneTimePayment f) <*> x = fmap f x

fromYearlyPayment (YearlyPayment a) = a
fromOneTimePayment (OneTimePayment a) = a
