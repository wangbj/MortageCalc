{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Test.QuickCheck.All

import Mortage
import Payment
import SimpleDTime

myloan = MortageLoan 250000.0 0.04 15 (DTime 2013 10 28)
mypay = Payment (Just 1000.0) (Just 500.0) (Just (YearlyPayment (10, 10000.0)) ) (Just (OneTimePayment ((DTime 2015 6 1), 20000.0)) )
defaultPay = Payment Nothing Nothing Nothing Nothing

main = do
  putStrLn "default payment"
  displayAmortization $ execMortage myloan defaultPay
  putStrLn "my payment"
  displayAmortization $ execMortage myloan mypay
