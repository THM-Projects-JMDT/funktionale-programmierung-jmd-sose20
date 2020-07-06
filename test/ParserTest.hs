module Main where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)
import Text.Parsec 
import SPLAbsyn
import SPLParser

{-
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Tests for the parser
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-}

-- do not change
main = defaultMain unitTests

-- new tests have to be added to this list
unitTests = testGroup "Unit tests" 
  [ testIntLitDec,
    testIntLitHex,
    testIntLitHexInvalid,
    testIntLitChar,
    testIntLitCharInvalid,
    testIdentOnlyUnderscore,
    testIdentStartsWithLowerCase,
    testIdentStartsWithUpperCase,
    testIdentStartsWithNum,
    testComment,
    testCommentInsideComment,
    testComments,
    testTypeDeclaration,
    testTypeDeclarationWithComments ]


-- IntLiteral Parser tests -----------------------

testIntLitDec = testCase "" $
  let expected = Right "3"
      actual = run pIntLit "3"
  in 
    assertEqual "" expected actual

testIntLitHex = testCase "" $
  let expected = Right "0x123abcDEF"
      actual = run pIntLit "0x123abcDEF"
  in 
    assertEqual "" expected actual

testIntLitHexInvalid = testCase "" $
  let expected = Right "0"
      actual = run pIntLit "0xg"
  in 
    assertEqual "" expected actual

testIntLitChar = testCase "" $
  let expected = Right "'\n'"
      actual = run pIntLit "'\n'"
  in 
    assertEqual "" expected actual

testIntLitCharInvalid = testCase "" $
  let res = run pIntLit "'aa'"
  in 
    assertBool "Only a single character is allowed inbetween single quotes" (isError res)
  

-- Identifier Parser tests -----------------------
testIdentOnlyUnderscore = testCase "" $ 
  let expected = Right "_"
      actual   = run pIdent "_"
  in 
    assertEqual "" expected actual

testIdentStartsWithNum = testCase "" $
  let res = run pIdent "2name"
  in 
    assertBool "Identifier must not start with a numeric character" (isError res)

testIdentStartsWithUpperCase = testCase "" $ 
  let expected = Right $ "Name_123"
      actual   = run pIdent "Name_123"
  in 
    assertEqual "" expected actual

testIdentStartsWithLowerCase = testCase "" $ 
  let expected = Right $ "nAME_123"
      actual   = run pIdent "nAME_123"
  in 
    assertEqual "" expected actual


-- Comment Parser tests --------------------------

testComment = testCase "" $ 
  let expected = Right $ " a comment"
      actual   = run pComment "// a comment\n"
  in 
    assertEqual "" expected actual

testCommentInsideComment = testCase "" $ 
  let expected = Right $ " a comment // inside a comment"
      actual   = run pComment "// a comment // inside a comment\n"
  in 
    assertEqual "" expected actual

testComments = testCase "" $
  let expected = Right $ ["A", "B", "C"]
      actual   = run pComments "//A\n\n  \n//B\n//C\n"
  in 
    assertEqual "" expected actual


-- Type Declaration Parser tests

testTypeDeclaration = testCase "" $ 
  let input    = "type vector = array[3] of int;"
      expected = Right $ (TypeDeclaration "vector" (ArrayTypeExpression "3" (NamedTypeExpression "int", [[]]), [[], [], [], [], []]), [[], [], [], []])
      actual   = run pTypeDeclaration input
  in 
    assertEqual "" expected actual 

testTypeDeclarationWithComments = testCase "" $
  let input = "type //A\n\n vector  //B\n = //C\n array //D\n[//E\n3//F\n]//G\n of //H\nint //I\n; //J\n//K\n" 
      expected = Right $ (TypeDeclaration "vector" (ArrayTypeExpression "3" (NamedTypeExpression "int", [["I"]]), [["D"], ["E"], ["F"], ["G"], ["H"]]), [["A"], ["B"], ["C"], ["J"]])
      actual   = run pTypeDeclaration input
  in 
    assertEqual "" expected actual


-- utility functions -----------------------------

run :: Parser a -> String -> Either ParseError a
run p = runParser p () ""  

isError :: Either a b -> Bool
isError (Left _) = True
isError _        = False