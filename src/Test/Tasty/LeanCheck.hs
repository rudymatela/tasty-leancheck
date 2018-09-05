module Test.Tasty.LeanCheck
  ( testProperty
  , LeanCheckTests (..)
  )
where

testProperty :: a
testProperty = undefined

newtype LeanCheckTests = LeanCheckTests Int
  deriving (Show, Eq, Ord)

-- TODO: instance IsOption LeanCheckTests

