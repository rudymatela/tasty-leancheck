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

import Test.Tasty.Providers
import Test.Tasty.Options
import Test.LeanCheck
import Data.Proxy

data TestResult = Ok
                | Failed String

toTestResult :: Maybe [String] -> TestResult
toTestResult Nothing = Ok
toTestResult (Just ce) = Failed $ unwords ce

-- | Create a 'Test' for a LeanCheck 'Testable' property.
testProperty :: Testable a => TestName -> a -> TestTree
testProperty name prop = singleTest name $ toTestResult $ counterExample 200 prop
-- TODO: change to the configured value
--       to do this, maybe I'll have to wrap resultiers in the TestResult

-- | Number of test cases for LeanCheck to generate.
newtype LeanCheckTests = LeanCheckTests Int
  deriving (Show, Eq, Ord)

instance IsOption LeanCheckTests where
  defaultValue = LeanCheckTests 200
  parseValue = fmap LeanCheckTests . safeRead
  optionName = return "leancheck-tests"
  optionHelp = return "Depth to use for leancheck tests"

instance IsTest TestResult where
  testOptions = return [Option (Proxy :: Proxy LeanCheckTests)]
  run _ Ok          _ = pure $ testPassed ""
  run _ (Failed ce) _ = pure $ testFailed $ unlines ["*** Failure:", ce, ""]
