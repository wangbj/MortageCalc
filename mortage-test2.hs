{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Test.QuickCheck.All
import Text.Printf

import SimpleDTime
import Payment
import Mortage

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

instance Arbitrary MortageLoan where
  arbitrary = do
    a <- choose (10000, 10000000) :: Gen Double
    b <- choose (1, 100) :: Gen Int
    c <- choose (1, 40) :: Gen Int
    y <- choose (2000, 2050) :: Gen Int
    m <- choose (1, 12) :: Gen Int
    d <- choose (1, 28) :: Gen Int
    return MortageLoan {totalAmount = a, interestRate = (fromIntegral b) / 1000.0, yearTerm = c, startDate = (DTime y m d) }

defaultPayment = Payment Nothing Nothing Nothing Nothing

totalPrincipalPaid :: Amortization -> Double
totalPrincipalPaid [] = 0.0
totalPrincipalPaid ((_, a):as) = monthlyPrincipal a + totalPrincipalPaid as

floatEq a b = abs (a - b) <= 0.0001

-- loose tests:

prop_principal_equality_defaultPayment loan = floatEq (totalPrincipalPaid (execMortage loan defaultPayment)) (totalAmount loan)
prop_numofterms_equality_defaultPayment loan = length (execMortage loan defaultPayment) == (12 * (yearTerm loan))
prop_term_correctness_defaultPayment loan = fst (last (execMortage loan defaultPayment) ) == (12 * (yearTerm loan))

prop_principal_equality loan pay = floatEq (totalPrincipalPaid (execMortage loan pay)) (totalAmount loan)
prop_term_correctness loan pay = fst (last (execMortage loan pay) ) <= (12 * yearTerm loan)

prop_less_interest loan pay = totalInterestPaid (execMortage loan pay) < totalInterestPaid (execMortage loan defaultPayment)
prop_principal_equality_2 loan pay = floatEq (totalPrincipalPaid (execMortage loan pay)) (totalPrincipalPaid (execMortage loan defaultPayment))
prop_principal_equality_3 loan pay1 pay2 = floatEq (totalPrincipalPaid (execMortage loan pay1)) (totalPrincipalPaid (execMortage loan pay2))

runTests = $quickCheckAll

main = runTests

