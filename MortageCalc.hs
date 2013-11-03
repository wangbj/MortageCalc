import System.Environment (getArgs)
import Control.Applicative
import Control.Monad

import Data.Maybe(fromJust)
import SimpleDTime
import Payment
import Mortage

constructArgs :: [String]-> [Maybe String]
constructArgs args =
  take 7 $ map (Just) args `mplus` b
  where b = [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]

tail' :: [a] -> Maybe [a]
tail' s
  | null s = Nothing
  | otherwise = Just (tail s)

break' f Nothing = Nothing
break' f (Just a) = Just (break f a)

read' a = case reads a of
  [(x, "")] -> Just x
  _ -> Nothing

readM Nothing = Nothing
readM (Just x) = read' x

parseDate :: String -> Maybe [Maybe Int]
parseDate dt =
  break' ( == '/') dt' >>= \(a, b) ->
  break' ( == '/') (tail' b) >>= \(c, d) ->
  break' ( == '/') (tail' d) >>= \(e, _) ->
  return ([read' a :: Maybe Int] ++ [read' c :: Maybe Int] ++ [read' e :: Maybe Int])
  where dt' = Just dt

readDate :: String -> Maybe (DTime)
readDate dt 
  | dt' == Nothing = Nothing
  | length dt'' /= 3 = Nothing
  | otherwise =
    let a = dt'' !! 0
        b = dt'' !! 1
        c = dt'' !! 2
    in a >>= \x ->
       b >>= \y ->
       c >>= \z -> return (DTime x y z)
  where dt' = parseDate dt
        dt'' = fromJust dt'

readDateGuarded' Nothing = readDate "2013/11/1"
readDateGuarded' (Just dt) = readDate dt

readOneTimePayment' :: Maybe String -> Maybe (OneTimePayment (DTime, Double))
readOneTimePayment' yp' =
  break' ( == ',' ) yp' >>= \(dt, m) ->
  (tail' m) >>= \m' ->
  (read' m' :: Maybe Double) >>= \m'' ->
  readDate dt >>= \dt' ->
  return $ OneTimePayment (dt', m'')  
  
readOneTimePayment :: String -> Maybe (OneTimePayment (DTime, Double))  
readOneTimePayment yp = readOneTimePayment' (Just yp)  

readYearlyPayment' :: Maybe String -> Maybe (YearlyPayment (Int, Double))
readYearlyPayment' yp' = 
  break' ( == ',') yp' >>= \(m, amt) ->
  (read' m :: Maybe Int) >>= \m' ->
  (tail' amt) >>= \amt' ->
  (read' amt' :: Maybe Double) >>= \amt'' ->
  return $ YearlyPayment (m', amt'')
  
readYearlyPayment :: String -> Maybe (YearlyPayment (Int, Double))  
readYearlyPayment yp = readYearlyPayment' (Just yp)

parseArgs :: [String] -> Maybe (MortageLoan, Payment)
parseArgs args = 
  let [arg1, arg2, arg3, arg4, arg5, arg6, arg7] = constructArgs args
      total = (readM arg1 :: Maybe Double)
      rate = (readM arg2 :: Maybe Double)
      term = (readM arg3 :: Maybe Int)
      madd = (readM arg4 :: Maybe Double)
      ypay = readYearlyPayment' arg5
      apay = readOneTimePayment' arg6
      start = readDateGuarded' arg7
  in total >>= \total' ->
     rate >>= \rate' ->
     term >>= \term' ->
     start >>= \start' ->
     return $ (MortageLoan total' rate' term' start', Payment Nothing madd ypay apay)
        
computeAmortization :: [String] -> Maybe Amortization
computeAmortization args = 
  (parseArgs args) >>= \(loan, pay) -> return $ execMortage loan pay

displayAmortization' :: Maybe Amortization -> IO ()
displayAmortization' Nothing = return ()
displayAmortization' (Just am) = displayAmortization am

displayTotalInterest' :: Maybe Amortization -> IO ()
displayTotalInterest' Nothing = return ()
displayTotalInterest' (Just am) = 
  print ("Total Interest Paid: " ++ show (totalInterestPaid am))
  
mainloop :: [String] -> IO ()
mainloop args = do
  let am = computeAmortization args
  displayAmortization' am
  displayTotalInterest' am
    
main = getArgs >>= mainloop
