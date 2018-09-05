module Test.Tasty.LeanCheck
  ( testProperty
  , LeanCheckTests (..)
  , module Test.LeanCheck
  )
where

import Test.Tasty.Providers
import Test.Tasty.Options
import Test.LeanCheck

testProperty :: a
testProperty = undefined

newtype LeanCheckTests = LeanCheckTests Int
  deriving (Show, Eq, Ord)

-- TODO: instance IsOption LeanCheckTests

