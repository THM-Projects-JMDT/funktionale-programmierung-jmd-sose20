module Main where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

main = defaultMain unitTests

unitTests =
  testGroup
    "Unit tests"
    [testA, testB]

testA =
  testCase "1 does not equal 2" $ assertEqual [] 1 2

testB =
  testCase "1+1 equals 2" $ assertEqual [] (1 + 1) 2