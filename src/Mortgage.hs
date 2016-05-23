{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
module Mortgage (
    runMortgage
  , runMortgageSimple
  , MortgageLoan(..)
  , AdditionalPayments(..)
  , AdditionalCosts(..)
  , MonthlyAmortized (..)
  , Mortgage
  , prettyShowAmortizedPayments
  , Money
    ) where

import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq)
import           Data.Time
import           Data.Maybe
import           Text.Printf
import           Control.Monad.RWS
import           Control.Monad.Fix
import           Control.Applicative
import           Mortgage.Money

data MortgageLoan = MortgageLoan {
    loanAmt   :: Money
  , loanApr   :: Double
  , loanYears :: Int
    } deriving Show

makeLoan :: Int -> Double -> Int -> MortgageLoan
makeLoan amt = MortgageLoan (fromIntegral amt)

data AdditionalPayments = AdditionalPayments {
    additionalMonthly :: Maybe Money
  , additionalYearly  :: Maybe (Int, Money)
  , additionalOnetime :: Maybe (Day, Money)
    } deriving Show

data AdditionalCosts = AdditionalCosts {
      cMonthlyHOA         :: Money
    , cMonthlyPropertyTax :: Money
    } deriving Show

data MonthlyAmortized = MonthlyAmortized {
    monthlyPrincipal      :: Money
  , monthlyInterest       :: Money
  , monthlyAdditionalCost :: Money
  , monthlyDate           :: Day
  } deriving (Eq, Show)

type AmortizedPayments = Seq MonthlyAmortized

today :: IO Day
today = fmap (localDay . zonedTimeToLocalTime) getZonedTime 

-- + 0.005 for round up.
monthlyPayment (MortgageLoan amt apr terms) = 0.005 + realToFrac amt * r * t / (t-1)
  where
    !r = apr / 100 / 12
    !t = (1 + r) ^ (12 * terms)

newtype Mortgage a = Mortgage {
  unMortgage :: RWST Env AmortizedPayments (Day, Money) IO a
  } deriving (  Functor, Applicative, Monad
              , MonadReader Env
              , MonadWriter AmortizedPayments
              , MonadState (Day, Money)
              , MonadIO )

toMortgagePaymentPlan :: Day -> MortgageLoan -> Maybe AdditionalCosts -> Maybe AdditionalPayments -> Env
toMortgagePaymentPlan day l@(MortgageLoan amt apr years) other pp = 
  Env { totalAmount        = amt
      , interestRate       = apr / 100 / 12
      , totalMonth         = years * 12
      , startFrom          = day
      , monthlyAmt         = realToFrac (monthlyPayment l)
      , monthlyAdditional  = pp >>= additionalMonthly
      , yearlyAdditional   = pp >>= additionalYearly
      , oneTimeAdditional  = pp >>= additionalOnetime
      , monthlyHOA         = maybe 0 cMonthlyHOA other
      , monthlyPropertyTax = maybe 0 cMonthlyPropertyTax other
      }

data Env = Env {
    totalAmount        :: Money
  , interestRate       :: Double
  , totalMonth         :: Int
  , startFrom          :: Day
  , monthlyAmt         :: Money
  , monthlyAdditional  :: Maybe Money
  , yearlyAdditional   :: Maybe (Int, Money)
  , oneTimeAdditional  :: Maybe (Day, Money)
  , monthlyHOA         :: Money
  , monthlyPropertyTax :: Money
  } deriving Show

interestAmt amt i = amt * i

middle (_, x, _) = x

thisMonthCanPay :: Day -> Env -> Money
thisMonthCanPay day env = monthlyAmt env + fromMaybe 0 (monthlyAdditional env) + amt1 + amt2
  where amt1 = case yearlyAdditional env of
          Nothing -> 0
          Just (m, amt) -> if m == (middle . toGregorian) day then amt else 0
        amt2 = case oneTimeAdditional env of
          Nothing -> 0
          Just (day', amt) -> let (y1, m1, _) = toGregorian day
                                  (y2, m2, _) = toGregorian day'
                              in if y1 == y2 && m1 == m2 then amt else 0

nextMonth :: Day -> Day
nextMonth day = fromGregorian y' m' d'
  where (y, m, d) = toGregorian day
        (y',m',d') | 1 + m  > 12 = (1+y, 1, d)
                   | 1 + m <= 12 = (y, 1+m, d)

runMonthly :: Mortgage ()
runMonthly = do
  (day, owe) <- get
  when (owe > 0) $ do
    env <- ask
    let totalThisMonthToPay = realToFrac (thisMonthCanPay day env)
        interest = realToFrac $ realToFrac owe * interestRate env
        principal = realToFrac $ totalThisMonthToPay - interest
    tell (Seq.singleton (MonthlyAmortized (min principal owe) interest (monthlyHOA env + monthlyPropertyTax env) day))    
    put (nextMonth day, max (owe - principal) 0) >> runMonthly

runMortgage :: MortgageLoan -> Maybe AdditionalCosts -> Maybe AdditionalPayments -> IO AmortizedPayments
runMortgage loan costs pp = today >>= \day -> 
    snd <$> execRWST (unMortgage runMonthly) (toMortgagePaymentPlan day loan costs pp) (day, loanAmt loan)

runMortgageSimple :: Int -> Double -> Int -> IO AmortizedPayments
runMortgageSimple amt apr years = runMortgage (makeLoan amt apr years) Nothing Nothing

prettyShowAmortizedPayments :: AmortizedPayments -> String
prettyShowAmortizedPayments (Seq.viewl -> Seq.EmptyL) = ""
prettyShowAmortizedPayments pps@(Seq.viewl -> p Seq.:< ps) = entries ++ summary ++ "\n"
  where summary = "total loan: " ++ show totalLoan ++ ", installments: " ++ show (Seq.length pps) ++ " " ++ ", total interest paid: " ++ show totalIntr
        totalIntr = getSum (foldMap (Sum . monthlyInterest) pps)
        totalLoan = getSum (foldMap (Sum . monthlyPrincipal) pps)
        entries = concatMap showEntry (zip [1..] (F.toList pps))
          where showEntry (k, MonthlyAmortized p i o d) = "# " ++ show k ++ ": " ++ show d ++ ", SubTotal: " ++ show t
                    ++ ", PrincipalPaid: " ++ show p ++ ", InterestPaid: " ++ show i ++ ", Other (HOA, Property tax: " ++ show o ++ ")\n"
                  where t = i + p + o
