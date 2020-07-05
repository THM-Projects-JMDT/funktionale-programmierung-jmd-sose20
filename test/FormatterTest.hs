module Main where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)
import Text.Parsec 
import SPLAbsyn
import SPLParser
import SPLFormatter

{-
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Tests for the formatter
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-}

-- do not change
main = defaultMain unitTests

-- new tests have to be added to this list
unitTests = testGroup "Unit tests" 
  [ ]



run :: Parser a -> String -> Either ParseError a
run p = runParser p () ""  

testFormat :: String -> Parser a -> PrettyPrinter a -> IO ()
testFormat s p f = putStrLn $ case run p s of 
                                Left err  -> "Parser failed: " ++ show err
                                Right r -> f defaultConfig 2 r