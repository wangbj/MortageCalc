{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Options.Applicative
import           Data.Maybe
import           Control.Monad
import           Numeric

import           Mortgage

data Config = Config {
    cTotalValue        :: Int
  , cDownPct           :: Double
  , cAPR               :: Double
  , cYears             :: Int
  , cPropertyTax       :: Double
  , cHOAPerMonth       :: Maybe Int
  , cAddtionalPerMonth :: Maybe Int
  , cAddtionalPerYear  :: Maybe (Int, Int)
    } deriving Show

cmdline :: Parser Config
cmdline = Config
    <$> argument auto (metavar "PropertyValue" <> help "Total Property value")
    <*> argument auto (metavar "DownPct" <> help "Down payment percentage (percentage, i.e: 20)")
    <*> argument auto (metavar "APR" <> help "APR (percentage, i.e: 5.00)")
    <*> argument auto (metavar "Years" <> help "loan in years")
    <*> argument auto (metavar "PropertyTax" <> help "anual property tax rate (percentage, i.e: 1.25)")
    <*> optional (option auto (long "hoa" <> short 'H' <> metavar "HOA" <> help "HOA per month"))
    <*> optional (option auto (long "monthlyAdd" <> short 'M' <> metavar "monthlyAdditional" <> help "monthly additional pay towards principal"))
    <*> ( join . liftA readYearly <$> optional (option str (long "yearlyAdd" <> short 'Y' <> metavar "YearlyAdditional" <> help "Yearly addtional payment at <month> towards principal <amt@month>")) )

readYearly :: String -> Maybe (Int, Int)
readYearly yearly = listToMaybe (readDec yearly) >>= \(amt, rest) ->
    if null rest then Nothing else listToMaybe (readDec (tail rest)) >>= \(mon, _) ->
    if mon < 1 || mon > 12 then Nothing else return (mon, amt)

greet :: Config -> IO ()
greet (Config value down apr years tax hoa madd yearly) = do
    let loan       = MortgageLoan (realToFrac $ fromIntegral value * (100 - down) / 100) apr years
        acosts     = AdditionalCosts (maybe 0 fromIntegral hoa) (realToFrac (fromIntegral value * tax / 100 / 12))
        additional = AdditionalPayments (fmap fromIntegral madd) (fmap (fmap fromIntegral) yearly) Nothing
    runMortgageDefault loan (Just acosts) (Just additional) >>= putStr . prettyShowAmortizedPayments
        
main :: IO ()
main = execParser opts >>= greet
  where opts = info (helper <*> cmdline)
            ( fullDesc
           <> progDesc "Mortgage Calculator"
           <> header "MortgageCalc" )
