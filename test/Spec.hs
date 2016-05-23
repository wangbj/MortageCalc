{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
import qualified Data.Sequence as Seq
import qualified Data.Foldable as F
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Data.Time
import Data.Monoid
import Control.Monad
import Data.Functor
import Control.Applicative

import GHC.Generics
import Mortgage

addYear n day = fromGregorian (y + fromIntegral n) m d
    where (y, m, d) = toGregorian day

instance Arbitrary Day where
    arbitrary = fromGregorian
        <$> choose (2000, 2040)
        <*> choose (1, 12)
        <*> choose (1, 31)

data SimpleLoan = SimpleLoan Int Double Int deriving Show

instance Arbitrary SimpleLoan where
    arbitrary = SimpleLoan
        <$> choose (0, 1000000000)
        <*> choose (0, 25)
        <*> choose (1, 50)

instance Arbitrary MortgageLoan where
    arbitrary = MortgageLoan
        <$> arbitrary
        <*> choose (0, 25)
        <*> choose (1, 50)

instance Arbitrary AdditionalPayments where
    arbitrary = AdditionalPayments
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary        

prop_full_term_pays_all (SimpleLoan amt apr years) = monadicIO $ do
    amortizedPayments <- run (F.toList <$> runMortgageSimple amt apr years)
    -- floating point could accumulate errors.
    assert $! length amortizedPayments `elem` [12 * years, 1 + 12 * years]

prop_full_term_pays_all_principal (SimpleLoan amt apr years) = monadicIO $ do
    amortizedPayments <- run (runMortgageSimple amt apr years)
    assert $! fromIntegral amt == getSum (foldMap (Sum . monthlyPrincipal) amortizedPayments)

return []
runTests = $quickCheckAll

main :: IO ()
main = void runTests
