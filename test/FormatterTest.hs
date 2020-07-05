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
  [testIntLit,
  testIntLitWithComSimple,
  testIntLitWithCom,
  testNamedVariable,
  testTypeDec,
  testTypeDecWithCom,
  testArrayTypeDec,
  testGlobalComment,
  testBiExpression,
  testdoubleBiExpression,
  testArrayAccess,
  testArrayAccessWithCom,
  testProDec,
  testProDecWithCom,
  testVarDec,
  testCallStmt,
  testAssStmt,
  testAssStmtWithCom,
  testAssStmtWithComSimple,
  testIfStmt,
  testIfElse,
  testWhileStmt,
  testWhileStmtWithCom,
  testWhileStmtWithComSimple
   ]


testIntLit = testCase "" $
  let expected = "5"
      actual = testFormat fExpression (IntLiteral "5",[[]])
  in 
    assertEqual "" expected actual

testIntLitWithCom = testCase "" $
  let expected = "5 //A\n"
      actual = testFormat fExpression (IntLiteral "5",[["A"]])
  in 
    assertEqual "" expected actual

testIntLitWithComSimple = testCase "" $
  let expected = "5 "
      actual = testFormatSimple fExpression (IntLiteral "5",[["A"]])
  in 
    assertEqual "" expected actual

testNamedVariable = testCase "" $
  let expected = "x"
      actual = testFormat fExpression (VariableExpression (NamedVariable "x",[[]]),[[]])
  in 
    assertEqual "" expected actual

testBiExpression = testCase "" $
  let expected = "47 < 45"
      actual = testFormat fExpression (BinaryExpression (Lt,[[]]) (IntLiteral "47",[[]]) (IntLiteral "45",[[]]),[[],[]])
  in 
    assertEqual "" expected actual

testdoubleBiExpression = testCase "" $
  let expected = "47 + x < 48"
      actual = testFormat fExpression (BinaryExpression (Lt,[[]]) (BinaryExpression (Plus,[[]]) (IntLiteral "47",[[]]) (VariableExpression (NamedVariable "x",[[]]),[[]]),[[],[]]) (IntLiteral "48",[[]]),[[],[],[]])
  in 
    assertEqual "" expected actual

testArrayAccess = testCase "" $
  let expected = "x[48]"
      actual = testFormat fExpression (VariableExpression (ArrayAccess (NamedVariable "x",[[]]) (IntLiteral "48",[[],[],[]]),[[],[],[]]),[[],[],[]])
  in 
    assertEqual "" expected actual

testArrayAccessWithCom = testCase "" $
  let expected = "x // A \n + 45"
      actual = testFormat fExpression (BinaryExpression (Plus,[[]]) (VariableExpression (NamedVariable "x",[[" A "]]),[[]]) (IntLiteral "45",[[]]),[[],[]])
  in 
    assertEqual "" expected actual

testTypeDec = testCase "" $
  let expected = "type myInt = int;\n"
      actual = testFormat fGlobalDeclaration (TypeDeclaration "myInt" (NamedTypeExpression "int",[[]]),[[],[],[],[]])
  in 
    assertEqual "" expected actual

testTypeDecWithCom = testCase "" $
  let expected = "type myInt // B \n= int;\n"
      actual = testFormat  fGlobalDeclaration (TypeDeclaration "myInt" (NamedTypeExpression "int",[[]]),[[],[" B "],[],[]])
  in 
    assertEqual "" expected actual

testArrayTypeDec = testCase "" $
  let expected = "type matrix = array[8] of array[8] of int;\n"
      actual = testFormat fGlobalDeclaration (TypeDeclaration "matrix" (ArrayTypeExpression "8" (ArrayTypeExpression "8" (NamedTypeExpression "int",[[]]),[[],[],[],[],[]]),[[],[],[],[],[]]),[[],[],[],[]])
  in 
    assertEqual "" expected actual

testGlobalComment = testCase "" $
  let expected = "// All kinds of type and procedure declarations spread across the program \n"
      actual = testFormat fGlobalDeclaration (GlobalComment " All kinds of type and procedure declarations spread across the program ",[])
  in 
    assertEqual "" expected actual

testProDec = testCase "" $
  let expected = "proc hide() {\n  ;\n}\n"
      actual = testFormat fGlobalDeclaration (ProcedureDeclaration "hide" [] [] [(EmptyStatement,[[]])],[[],[],[],[],[],[]])
  in 
    assertEqual "" expected actual 

testProDecWithCom = testCase "" $
  let expected = "proc hide( //ich\n) //bin\n{ //ein\n  ;\n}  //Kommentar\n"
      actual = testFormat fGlobalDeclaration (ProcedureDeclaration "hide" [] [] [(EmptyStatement,[[]])],[[],[],["ich"],["bin"],["ein"],["Kommentar"]])
  in 
    assertEqual "" expected actual 

testVarDec = testCase "" $
  let expected = "var i: int;\n"
      actual = testFormat fVariableDeclaration (VariableDeclaration "i" (NamedTypeExpression "int",[[]]),[[],[],[],[]])
  in 
    assertEqual "" expected actual 

testCallStmt = testCase "" $
  let expected = "callRecursive(i, j);\n"
      actual = testFormat fStatement (CallStatement "callRecursive" [(VariableExpression (NamedVariable "i",[[]]),[[]]),(VariableExpression (NamedVariable "j",[[]]),[[],[]])],[[],[],[],[]])
  in 
    assertEqual "" expected actual

testAssStmt = testCase "" $
  let expected = "i := 1 - 2 - 3;\n"
      actual = testFormat fStatement (AssignStatement (NamedVariable "i",[[]]) (BinaryExpression (Minus,[[]]) (BinaryExpression (Minus,[[]]) (IntLiteral "1",[[]]) (IntLiteral "2",[[]]),[[],[]]) (IntLiteral "3",[[]]),[[],[],[]]),[[],[]])
  in 
    assertEqual "" expected actual

testAssStmtWithCom = testCase "" $
  let expected = "m[-i][8] := // B \n135;\n"
      actual = testFormat fStatement (AssignStatement (ArrayAccess (ArrayAccess (NamedVariable "m",[[]]) (Negative (VariableExpression (NamedVariable "i",[[]]),[[]]),[[],[],[]]),[[],[],[]]) (IntLiteral "8",[[],[],[]]),[[],[],[]]) (IntLiteral "135",[[]]),[[" B "],[]])
  in 
    assertEqual "" expected actual

testAssStmtWithComSimple = testCase "" $
  let expected = "m[-i][8] := 135;\n"
      actual = testFormatSimple fStatement (AssignStatement (ArrayAccess (ArrayAccess (NamedVariable "m",[[]]) (Negative (VariableExpression (NamedVariable "i",[[]]),[[]]),[[],[],[]]),[[],[],[]]) (IntLiteral "8",[[],[],[]]),[[],[],[]]) (IntLiteral "135",[[]]),[[" B "],[]])
  in 
    assertEqual "" expected actual

testIfStmt = testCase "" $
  let expected = "if (n <= 10) ;\n"
      actual = testFormat fStatement (IfStatement (BinaryExpression (Le,[[]]) (VariableExpression (NamedVariable "n",[[]]),[[]]) (IntLiteral "10",[[]]),[[],[]]) (EmptyStatement,[[]]) Nothing,[[],[],[],[]])
  in 
    assertEqual "" expected actual

testIfElse = testCase "" $
  let expected = "if (n <= 10) ;\nelse ;\n"
      actual = testFormat fStatement (IfStatement (BinaryExpression (Le,[[]]) (VariableExpression (NamedVariable "n",[[]]),[[]]) (IntLiteral "10",[[]]),[[],[]]) (EmptyStatement,[[]]) (Just (EmptyStatement,[[]])),[[],[],[],[]])
  in 
    assertEqual "" expected actual

testWhileStmt = testCase "" $
  let expected = "while (n <= 10) i := 10;\n"
      actual = testFormat fStatement (WhileStatement (BinaryExpression (Le,[[]]) (VariableExpression (NamedVariable "n",[[]]),[[]]) (IntLiteral "10",[[]]),[[],[]]) (AssignStatement (NamedVariable "i",[[]]) (IntLiteral "10",[[]]),[[],[]]),[[],[],[]])
  in 
    assertEqual "" expected actual

testWhileStmtWithCom = testCase "" $
  let expected = "while (n <= 10) {\n  i := 10; //XXX\n}\n"
      actual = testFormat fStatement (WhileStatement (BinaryExpression (Le,[[]]) (VariableExpression (NamedVariable "n",[[]]),[[]]) (IntLiteral "10",[[]]),[[],[]]) (CompoundStatement [(AssignStatement (NamedVariable "i",[[]]) (IntLiteral "10",[[]]),[[],["XXX"]])],[[],[]]),[[],[],[]])
  in 
    assertEqual "" expected actual

testWhileStmtWithComSimple = testCase "" $
  let expected = "while (n <= 10) {\n  i := 10; //XXX\n}\n"
      actual = testFormatSimple fStatement (WhileStatement (BinaryExpression (Le,[[]]) (VariableExpression (NamedVariable "n",[[]]),[[]]) (IntLiteral "10",[[]]),[[],[]]) (CompoundStatement [(AssignStatement (NamedVariable "i",[[]]) (IntLiteral "10",[[]]),[[],["XXX"]])],[[],[]]),[[],[],[]])
  in 
    assertEqual "" expected actual

testConfig = Config Space 2 False True Linux
testConfigSimple = Config Space 2 False False Linux

testFormat s f = s testConfig 0 f
testFormatSimple s f = s testConfigSimple 0 f
