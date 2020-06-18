module Main where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

{-
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Tests for the formatter
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-}

-- do not change
main = defaultMain unitTests

-- new tests have to be added to this list
unitTests = testGroup "Unit tests" 
  [ testA
  , testB ]


-- test cases ------------------------------------

testA = testCase "1 does not equal 2" $ assertEqual [] 1 2

testB = testCase "1+1 equals 2" $ assertEqual [] (1 + 1) 2
