module Mortage (
  MortageLoan(..),
  MonthlyAmortization(..),
  Amortization,
  execMortage,
  totalInterestPaid,
  displayAmortization
  ) where

import Data.Maybe(fromJust)
import Control.Monad
import Control.Monad.State
import Control.Applicative
import Text.Printf

import SimpleDTime
import Payment

data MortageLoan = MortageLoan {
  totalAmount :: Double,
  interestRate :: Double,
  yearTerm :: Int,
  startDate :: DTime
  } deriving (Show)

data MonthlyAmortization = MonthlyAmortization {
  monthlyPrincipal :: Double,
  monthlyInterest :: Double,
  currentDate :: DTime
  } deriving (Eq, Show)

type Amortization = [(Int, MonthlyAmortization)]

validLoan :: MortageLoan -> Bool
validLoan (MortageLoan a b c _)
 | a < 0.000001 = False
 | b < 0.000001 = False
 | b >= 1.0 = False
 | c < 1 = False
 | otherwise = True

addPrincipal :: MonthlyAmortization -> Double -> Maybe Double -> MonthlyAmortization
addPrincipal am rem p
  | p == Nothing = am
  | otherwise =
    MonthlyAmortization (( min (fromJust p) rem) + monthlyPrincipal am) (monthlyInterest am) (currentDate am)
  
monthlyPaymentLoan :: MortageLoan -> Maybe Double
monthlyPaymentLoan loan =
  if validLoan loan then
    Just ( (totalAmount loan) * r * t / (t - 1))
  else Nothing
  where    r = (interestRate loan) / 12
           t = (1.0 + r) ^ ( 12 * (yearTerm loan))
           
monthlyPaymentLoan' :: MortageLoan -> Double
monthlyPaymentLoan' loan =
  (totalAmount loan) * r * t / (t - 1)
  where    r = (interestRate loan) / 12
           t = (1.0 + r) ^ ( 12 * (yearTerm loan))

fixupPayment :: MortageLoan -> Payment -> Payment
fixupPayment loan pay =
  Payment (monthlyPaymentLoan loan) (monthlyAdditional pay)
           (yearlyPayment pay) (oneTimePayment pay)
  
yearlyMatch :: MortageLoan -> Payment -> Int -> Bool
yearlyMatch loan pay term =
  month (addMonth (startDate loan) term) ==  month (startDate loan)

oneTimeMatch' :: MortageLoan -> Payment -> Int -> Maybe (OneTimePayment Bool)
oneTimeMatch' loan pay term =
  (apay >>= \y -> return $
  y >>= \x@(d,_) -> return $
   (  (year thisTerm == year d ) &&
             (month thisTerm == month d ) ) )
  where
    thisTerm = addMonth (startDate loan) term
    apay = oneTimePayment pay

oneTimeMatch :: MortageLoan -> Payment -> Int -> Bool
oneTimeMatch loan pay term = oneTimeMatch' loan pay term == Just (OneTimePayment True)

nthTerm :: MortageLoan -> Int -> DTime
nthTerm loan term =
  addMonth (startDate loan) term

monthlyAll :: Payment -> Double
monthlyAll pay =
  let a = (+) <$> m1 <*> m2
  in fromJust a
  where m1 = (monthlyPayment pay) `mplus` m3
        m2 = monthlyAdditional pay `mplus` m3
        m3 = Just (0.0 :: Double)

payMonthly :: MortageLoan -> Payment -> Int -> Double -> Maybe MonthlyAmortization
payMonthly loan pay term remains
  | remains <= 0.0000001 = Nothing
  | otherwise =
    let mir = (interestRate loan) / 12
        mtt = (yearTerm loan) * 12
        mintr = remains * mir
        mpp =  (monthlyAll pay) - mintr
        rem = remains - mpp
        in Just (MonthlyAmortization mpp mintr ((nthTerm loan term)))

payYearly :: MortageLoan -> Payment -> Int -> Double -> Maybe Double
payYearly loan pay term remains
  | remains <= 0.0000001 = Nothing
  | not (yearlyMatch loan pay term) = Nothing
  | otherwise =
     let pp = (yearlyPayment pay >>= \x -> return $
                                x >>= \y@(a, b) -> return $
                                ( (min b remains) ) )
     in fmap fromYearlyPayment pp

payOneTime :: MortageLoan -> Payment -> Int -> Double -> Maybe Double
payOneTime loan pay term remains
  | remains <= 0.0000001 = Nothing
  | not (oneTimeMatch loan pay term) = Nothing
  | otherwise =
     let pp = (oneTimePayment pay >>= \x -> return $
                                x >>= \y@(a, b) -> return $
                                ( (min b remains) ) )
     in fmap fromOneTimePayment pp

scheduledPayment :: MortageLoan -> Payment -> Int -> Double -> Maybe Double
scheduledPayment loan pay term remains =
  let apay = payOneTime loan pay term remains
      ypay = payYearly loan pay term remains
  in if apay /= Nothing && ypay /= Nothing then
       (+) <$> apay <*> ypay
     else apay `mplus` ypay

ppayHelper :: Maybe Double -> Double
ppayHelper ppay =
  if ppay == Nothing then 0.0
  else fromJust ppay
       
execMortage' :: MortageLoan -> Payment -> Int -> Double -> Amortization
execMortage' loan pay term remains
  | remains <= 0.001 = []
  | otherwise =
    let spay = scheduledPayment loan pay term remains
        cpay = payMonthly loan pay term remains -- must pay intr first
        in if cpay == Nothing then []
           else let cpay' = fromJust cpay
                    ppay = min remains ( (ppayHelper spay) + (monthlyPrincipal cpay') )
                    rem' = remains - ppay
                in (term, MonthlyAmortization ppay (monthlyInterest cpay') (nthTerm loan term)) : (execMortage' loan pay (succ term) rem')

execMortage :: MortageLoan -> Payment -> Amortization
execMortage loan pay =
  execMortage' loan (fixupPayment loan pay) 1 (totalAmount loan)
  
totalInterestPaid' :: Amortization -> Maybe Double
totalInterestPaid' [] = Just 0.0
totalInterestPaid' ((_, a):as) = Just $ monthlyInterest a + fromJust (totalInterestPaid' as)

totalInterestPaid :: Amortization -> Double
totalInterestPaid [] = 0.0
totalInterestPaid ((_, a):as) = monthlyInterest a + totalInterestPaid as
  
-- utility function
showDouble :: Double -> String
showDouble x = printf "%.2f" x
  
showAmortization :: Amortization -> [String]
showAmortization [] = []
showAmortization (x@(t, am):xs) = 
  (show (year (currentDate am)) ++ "/" ++ show (month (currentDate am)) ++" term(" ++ show t ++ ")" ++ " Payment: " ++ (showDouble payment) ++
   " Principal paid: " ++ (showDouble principalPaid) ++
   " Interest paid: " ++ (showDouble interestPaid) )
   : (showAmortization xs)
  where principalPaid = monthlyPrincipal am
        interestPaid = monthlyInterest am
        payment = principalPaid + interestPaid

displayAmortization :: Amortization -> IO ()
displayAmortization am =
  mapM_ (putStrLn) am'
  where am' = showAmortization am
