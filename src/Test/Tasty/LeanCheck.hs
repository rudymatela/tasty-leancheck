-- |
-- Module      : Test.LeanCheck
-- Copyright   : (c) 2018 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- LeanCheck support for the Tasty test framework.
--
-- TODO: add example of use here, duplicate on README and cabal summary
module Test.Tasty.LeanCheck
  ( testProperty
  , LeanCheckTests (..)
  , module Test.LeanCheck
  )
where

import Test.Tasty.Providers hiding (Result)
import Test.Tasty.Options
import Test.LeanCheck
import Data.Proxy
import Control.Exception (SomeException, catch, evaluate)

newtype Results = Results [([String],Bool)]

data Result = OK        Int
            | Falsified Int [String]
            | Exception Int [String] String
  deriving (Eq, Show)

-- | Create a 'Test' for a LeanCheck 'Testable' property.
testProperty :: Testable a => TestName -> a -> TestTree
testProperty name = singleTest name . Results . results

-- | Number of test cases for LeanCheck to generate.
newtype LeanCheckTests = LeanCheckTests Int
  deriving (Show, Eq, Ord)

instance IsOption LeanCheckTests where
  defaultValue = LeanCheckTests 200
  parseValue = fmap LeanCheckTests . safeRead
  optionName = return "leancheck-tests"
  optionHelp = return "Depth to use for leancheck tests"

instance IsTest Results where
  testOptions = return [Option (Proxy :: Proxy LeanCheckTests)]
  run opts results _ = resultIO m results >>= \r -> pure $
    case r of
      OK n             -> testPassed $ "+++ OK, passed " ++ show n ++ " tests"
                                    ++ takeWhile (\_ -> n < m) " (exhausted)"
                                    ++ "."
      Falsified i ce   -> testFailed $ "*** Failed! Falsifiable (after "
                                    ++ show i ++ " tests):\n" ++ joinArgs ce
      Exception i ce e -> testFailed $ "*** Failed! Exception '" ++ e
                                    ++ "' (after " ++ show i ++ " tests):\n"
                                    ++ joinArgs ce
    where
    LeanCheckTests m = lookupOption opts

resultsIO :: Int -> Results -> [IO Result]
resultsIO n (Results results) = zipWith torio [1..] $ take n results
  where
    tor i (_,True) = OK i
    tor i (as,False) = Falsified i as
    torio i r@(as,_) = evaluate (tor i r)
       `catch` \e -> let _ = e :: SomeException
                     in return (Exception i as (show e))

resultIO :: Int -> Results -> IO Result
resultIO n = computeResult . resultsIO n
  where
  computeResult []  = error "resultIO: no results, empty Listable enumeration?"
  computeResult [r] = r
  computeResult (r:rs) = r >>= \r -> case r of
                                     (OK _) -> computeResult rs
                                     _      -> return r

-- joins the counter-example arguments
joinArgs :: [String] -> String
joinArgs ce | any ('\n' `elem`) ce = unlines $ map chopBreak ce
            | otherwise            = unwords ce

-- chops a line break at the end if there is any
chopBreak :: String -> String
chopBreak [] = []
chopBreak ['\n'] = []
chopBreak (x:xs) = x:chopBreak xs
