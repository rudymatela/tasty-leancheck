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

-- | Create a 'Test' for a LeanCheck 'Testable' property
testProperty :: Testable a => TestName -> a -> TestTree
testProperty name prop = singleTest name $ holds 100 prop
-- TODO: change to the configured value

newtype LeanCheckTests = LeanCheckTests Int
  deriving (Show, Eq, Ord)

-- TODO: instance IsOption LeanCheckTests
-- TODO: instance IsTest Bool ??
instance IsOption LeanCheckTests where
  defaultValue = LeanCheckTests 200
  parseValue = fmap LeanCheckTests . safeRead
  optionName = return "leancheck-depth"
  optionHelp = return "Depth to use for leancheck tests"

instance IsTest Bool where
  testOptions = return [Option (Proxy :: Proxy LeanCheckTests)]
  run opts prop callback = undefined
