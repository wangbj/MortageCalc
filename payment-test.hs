{-# LANGUAGE TemplateHaskell #-}

import Payment
import SimpleDTime

import Test.QuickCheck
import Test.QuickCheck.All

import Control.Applicative
import Control.Monad

import Data.Maybe(fromJust)

data MortageLoan = MortageLoan {
  totalAmount :: Double,
  interestRate :: Double,
  yearTerm :: Int,
  startDate :: DTime
  } deriving (Show)

myloan = MortageLoan {totalAmount = 250000.0, interestRate = 0.04, yearTerm = 15, startDate = (DTime 2013 10 28)}

aa = Payment {monthlyPayment = Just 1000.0, monthlyAdditional = Just 100.0, yearlyPayment = Just (YearlyPayment (10, 10000.0)), oneTimePayment = Just (OneTimePayment ((DTime 2015 10 1), 20000.0) )}

bb = Payment {monthlyPayment = Just 1000.0, monthlyAdditional = Nothing, yearlyPayment = Nothing, oneTimePayment = Nothing}

dt1 = DTime 2015 10 1

instance Arbitrary Payment where
  arbitrary = do
    m <- choose (1, 12) :: Gen Int
    y <- choose (1970, 2050) :: Gen Int
    d <- choose (1, 28) :: Gen Int
    f0 <- choose (1000, 10000) :: Gen Double
    f1 <- choose(100, 10000) :: Gen Double
    f2 <- choose(1000, 100000000) :: Gen Double
    f3 <- choose(1000, 100000000) :: Gen Double
    return Payment { monthlyPayment = Just f0, monthlyAdditional = Just f1, yearlyPayment = Just (YearlyPayment (m, f2)), oneTimePayment = Just (OneTimePayment ((DTime y m d), f3) ) }

prop_payment_monthly_test1 pay =
  (mpay >>= (\x -> return $ (+100.0) x) ) < (mpay >>= (\x -> return $ (+101.0) x) )
  where mpay = monthlyAdditional pay

prop_payment_monthly_test2 pay =
  monthlyAdditional pay /= Nothing

prop_payment_yearly_test1 pay =
  (ypay >>= \x -> return $
                  x >>= \y@(a, b) -> return $ (a, b + 1.0) )
  /= ypay
  where ypay = yearlyPayment pay

prop_payment_yearly_test2 pay =
  (ypay >>= \x -> return $
                  x >>= \y@(a, b) -> return $ (succ a, b) )
  /= ypay
  where ypay = yearlyPayment pay

prop_payment_onetime_test1 pay =
  (ypay >>= \x -> return $
                  x >>= \y@(a, b) -> return $ (a, b + 1.0) )
  /= ypay
  where ypay = oneTimePayment pay

prop_payment_onetime_test2 pay =
  (ypay >>= \x -> return $
                  x >>= \y@(a, b) -> return $ (succMonth a, b) )
  /= ypay
  where ypay = oneTimePayment pay

runTests = $quickCheckAll

main = runTests