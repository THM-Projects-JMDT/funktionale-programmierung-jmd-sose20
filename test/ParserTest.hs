module Main where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)
import Text.Parsec 
import Absyn
import Parser

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
    testBiExpr,
    testdoubleBiExpr,
    testIdentStartsWithNum,
    testComment,
    testCommentInsideComment,
    testComments,
    testTypeDeclaration,
    testTypeDeclarationWithComments,
    testNamedTypExpr,
    testArrTypExpr,
    testArrTypExprWithCom,
    testNamedVar,
    testArrAccess,
    testSimProDec,
    testProDec,
    testVarDec,
    testVarDecWithCom,
    testAssStmt,
    testCallStmt,
    testWhileStmt,
    testIfStmt,
    testElseStmt,
    testEmptStmt,
    testComStmt
    ]


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

-- Binary Expression Parser tests --------------------------    

testBiExpr= testCase "" $ 
  let expected = Right $ (BinaryExpression (Plus,[[]]) (IntLiteral "5",[[]]) (IntLiteral "5",[[]]),[[],[]])
      actual   = run pExpression "5+5"
  in 
    assertEqual "" expected actual


testdoubleBiExpr= testCase "" $ 
  let expected = Right $ (BinaryExpression (Lt,[[]]) (BinaryExpression (Plus,[[]]) (IntLiteral "5",[[]]) (IntLiteral "5",[[" A "]]),[[],[" A "]]) (Negative (IntLiteral "20",[[]]),[[]]),[[],[" A "],[]])
      actual   = run pExpression "5+5 // A \n < -20"
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

-- Type Expression Parser tests

testNamedTypExpr= testCase "" $ 
  let input = "int"
      expected = Right $ (NamedTypeExpression "int",[[]])
      actual   = run pNamedTypeExpression input
  in 
    assertEqual "" expected actual

testArrTypExpr= testCase "" $ 
  let input = "array [8] of array [8] of int"
      expected = Right $ (ArrayTypeExpression "8" (ArrayTypeExpression "8" (NamedTypeExpression "int",[[]]),[[],[],[],[],[]]),[[],[],[],[],[]])
      actual   = run pArrayTypeExpression input
  in 
    assertEqual "" expected actual

testArrTypExprWithCom= testCase "" $ 
  let input = "array [8] of array [8] // A \n of int"
      expected = Right $ (ArrayTypeExpression "8" (ArrayTypeExpression "8" (NamedTypeExpression "int",[[]]),[[],[],[],[" A "],[]]),[[],[],[],[],[]])
      actual   = run pArrayTypeExpression input
  in 
    assertEqual "" expected actual

-- Variables Parser tests
testNamedVar= testCase "" $ 
  let input = "x"
      expected = Right $ (NamedVariable "x",[[]])
      actual   = run pVariable input
  in 
    assertEqual "" expected actual

testArrAccess= testCase "" $ 
  let input = "m[-i][8]"
      expected = Right $ (ArrayAccess (ArrayAccess (NamedVariable "m",[[]]) (Negative (VariableExpression (NamedVariable "i",[[]]),[[]]),[[],[],[]]),[[],[],[]]) (IntLiteral "8",[[],[],[]]),[[],[],[]])
      actual   = run pVariable input
  in 
    assertEqual "" expected actual

-- Procedure Declaritions Parser tests

testSimProDec= testCase "" $ 
  let input = "proc hide () {;}"
      expected = Right $ (ProcedureDeclaration "hide" [] [] [(EmptyStatement,[[]])],[[],[],[],[],[],[]])
      actual   = run pProcedureDeclaration input
  in 
    assertEqual "" expected actual

testProDec= testCase "" $ 
  let input = "proc manyargs(i : int, j : myInt, ref k:tensor, ref l : myInt) {;}"
      expected = Right $ (ProcedureDeclaration "manyargs" [(ParameterDeclaration "i" (NamedTypeExpression "int",[[]]) False,[[],[],[]]),(ParameterDeclaration "j" (NamedTypeExpression "myInt",[[]]) False,[[],[],[],[]]),(ParameterDeclaration "k" (NamedTypeExpression "tensor",[[]]) True,[[],[],[],[]]),(ParameterDeclaration "l" (NamedTypeExpression "myInt",[[]]) True,[[],[],[],[]])] [] [(EmptyStatement,[[]])],[[],[],[],[],[],[]])
      actual   = run pProcedureDeclaration input
  in 
    assertEqual "" expected actual

-- Variable Declaritions Parser tests

testVarDec= testCase "" $ 
  let input = "var i : int;"
      expected = Right $ (VariableDeclaration "i" (NamedTypeExpression "int",[[]]),[[],[],[],[]])
      actual   = run pVariableDeclaration input
  in 
    assertEqual "" expected actual

testVarDecWithCom= testCase "" $ 
  let input = "var i // Kommentar \n : int;"
      expected = Right $ (VariableDeclaration "i" (NamedTypeExpression "int",[[]]),[[],[" Kommentar "],[],[]])
      actual   = run pVariableDeclaration input
  in 
    assertEqual "" expected actual

-- Statement Parser tests

testAssStmt= testCase "" $ 
  let input = "i := 1-2-3;"
      expected = Right $ (AssignStatement (NamedVariable "i",[[]]) (BinaryExpression (Minus,[[]]) (BinaryExpression (Minus,[[]]) (IntLiteral "1",[[]]) (IntLiteral "2",[[]]),[[],[]]) (IntLiteral "3",[[]]),[[],[],[]]),[[],[]])
      actual   = run pStatement input
  in 
    assertEqual "" expected actual

testCallStmt= testCase "" $ 
  let input = "ausgabe(1);"
      expected = Right $ (CallStatement "ausgabe" [(IntLiteral "1",[[]])],[[],[],[],[]])
      actual   = run pStatement input
  in 
    assertEqual "" expected actual

testWhileStmt= testCase "" $ 
  let input = "while (i<=3) {;}"
      expected = Right $ (WhileStatement (BinaryExpression (Le,[[]]) (VariableExpression (NamedVariable "i",[[]]),[[]]) (IntLiteral "3",[[]]),[[],[]]) (CompoundStatement [(EmptyStatement,[[]])],[[],[]]),[[],[],[]])
      actual   = run pStatement input
  in 
    assertEqual "" expected actual

testIfStmt= testCase "" $ 
  let input = "if (i=j) ;"
      expected = Right $  (IfStatement (BinaryExpression (Eq,[[]]) (VariableExpression (NamedVariable "i",[[]]),[[]]) (VariableExpression (NamedVariable "j",[[]]),[[]]),[[],[]]) (EmptyStatement,[[]]) Nothing,[[],[],[],[]])
      actual   = run pStatement input
  in 
    assertEqual "" expected actual

testElseStmt= testCase "" $ 
  let input =  "if (i=j) ; else ;"
      expected = Right $ (IfStatement (BinaryExpression (Eq,[[]]) (VariableExpression (NamedVariable "i",[[]]),[[]]) (VariableExpression (NamedVariable "j",[[]]),[[]]),[[],[]]) (EmptyStatement,[[]]) (Just (EmptyStatement,[[]])),[[],[],[],[]])
      actual   = run pStatement input
  in 
    assertEqual "" expected actual  

testEmptStmt= testCase "" $ 
  let input = ";"
      expected = Right $ (EmptyStatement,[[]])
      actual   = run pStatement input
  in 
    assertEqual "" expected actual    

testComStmt= testCase "" $ 
  let input = "// Ich bin Kommentar \n"
      expected = Right $ (StatementComment " Ich bin Kommentar ",[])
      actual   = run pStatement input
  in 
    assertEqual "" expected actual  


-- utility functions -----------------------------

run :: Parser a -> String -> Either ParseError a
run p = runParser p () ""  

isError :: Either a b -> Bool
isError (Left _) = True
isError _        = False