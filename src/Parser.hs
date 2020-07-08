{-|
Module      : Parser
Description : A Parser for SPL.

This module provides functions for parsing SPL source code into an AST-representation.
The functions are based on monadic parser combinators provided by the Parsec library.
-}


module Parser where

import Text.Parsec
import Text.Parsec.Expr
import Data.Char

import Absyn


----------------------------------------------------
-- * Parsers
----------------------------------------------------

-- |Base type alias for the parser functions
type Parser a = Parsec String () a -- for convenience


-- ** Token Parsers 
-----------------------------------------------

-- comma, semicolon, colon ---------------

pComma, pSemic, pColon :: Parser Char
pComma  = char ','   
pSemic  = char ';'    
pColon  = char ':'    


-- keywords ------------------------------

pElse, pWhile, pRef, pIf, pOf, pType, pProc, pArray, pVar :: Parser String
pElse  = string "else"   
pWhile = string "while"  
pRef   = string "ref"    
pIf    = string "if"  
pOf    = string "of"     
pType  = string "type"   
pProc  = string "proc"   
pArray = string "array"  
pVar   = string "var"   


-- brackets, parantheses -----------------

pLParen, pRParen, pLCurl, pRCurl, pLBrack, pRBrack :: Parser Char
pLParen = char '('       
pRParen = char ')'      
pLCurl  = char '{'      
pRCurl  = char '}'      
pLBrack = char '['      
pRBrack = char ']'      


-- operators -----------------------------

pLT, pNE, pASGN, pPLUS, pSLASH, pSTAR, pGT, pLE, pMINUS, pGE, pEQ :: Parser Op
pLT    = char '<' >> return Lt   
pNE    = char '#' >> return Ne       
pASGN  = string ":=" >> return Asgn  
pPLUS  = char '+' >> return Plus   
pSLASH = char '/' >> return Slash   
pSTAR  = char '*' >> return Star    
pGT    = char '>' >> return Gt   
pLE    = string "<=" >> return Le  
pMINUS = char '-' >> return Minus   
pGE    = string ">=" >> return Ge  
pEQ    = char '=' >> return Eq

pOpPoint :: Parser Op
pOpPoint = pSLASH <|> pSTAR

pOpDash :: Parser Op
pOpDash = pPLUS <|> pMINUS

pOpCompare :: Parser Op
pOpCompare = try pLE <|> try pGE <|> pLT <|> pGT <|> pNE <|> pEQ 


-- integer literals ----------------------

pIntLit :: Parser IntString
pIntLit = try pHexLit 
          <|> pCharLit 
          <|> pDecLit

pHexLit :: Parser IntString
pHexLit = do 
  x  <- string "0x"
  xs <- many1 hexDigit
  return $ x ++ xs

pCharLit :: Parser IntString
pCharLit = do 
  n <- between (char '\'') (char '\'') (try (string "\\n" >> return '\n') <|> anyChar)
  return $ "'" ++ [n] ++ "'"

pDecLit :: Parser IntString
pDecLit = do 
  ds <- many1 digit 
  return ds


-- identifiers ---------------------------
 
pIdent :: Parser String
pIdent = do
  x <- upper <|> lower <|> char '_'
  xs <- many $ alphaNum <|> char '_'
  return (x : xs)


-- ** Comments
-----------------------------------------------

-- |Parses a single comment and following spaces in the next line.
--
-- The parser saves comments in the AST, so they are available at the pretty printing step.
pComment :: Parser Comment 
pComment = try $ do 
  string "//"
  cs <- manyTill anyChar (try $ (endOfLine >> return ()) <|> eof)
  spacesL
  return cs


-- |Parses an optional comment and returns an empty list (if there is no comment) or a singleton (if there is one).
pCommentOptional :: Parser [Comment]
pCommentOptional = do 
  la <- lookAhead $ optionMaybe anyChar
  cm <- case la of 
             Just '\n' -> endOfLine >> spacesL >> return Nothing
             Just '\r' -> endOfLine >> spacesL >> return Nothing
             Just '/'  -> Just <$> pComment
             otherwise -> return Nothing
  let cs = case cm of 
             Nothing -> []
             Just c  -> [c]
  return cs

-- |Parses a sequence of comments with optional spaces inbetween and following spaces.
pComments :: Parser [Comment]
pComments = many $ pComment << spacesN



-- ** AST nodes
-----------------------------------------------

-- Program -------------------------------

-- |Parses SPL source code into the corresponding AST.
pProgram :: Parser Program
pProgram = do
  spacesL
  glob <- many $ pGlobalEmptyLine 
             <|> pGlobalComment 
             <|> pTypeDeclaration 
             <|> pProcedureDeclaration
  spacesN >> eof
  return (Program glob)


-- Global Empty Line --------------------

-- |The Parser saves empty lines separating global declarations in the AST, this function fullfills this purpose.
pGlobalEmptyLine :: Parser (Commented GlobalDeclaration)
pGlobalEmptyLine = endOfLine >> spacesL >> return (GlobalEmptyLine, [])


-- Global Comment

-- |The Parser saves comments appearing inbetween global declarations in the AST, this function fullfills this purpose.
pGlobalComment :: Parser (Commented GlobalDeclaration)
pGlobalComment = do 
  c <- pComment
  return (GlobalComment c, [])


-- Type Declaration -----------------------

pTypeDeclaration :: Parser (Commented GlobalDeclaration)
pTypeDeclaration = do 
  pType >> spacesN
  cs1 <- pComments 
  id <- pIdent << spacesN
  cs2 <- pComments 
  pEQ >> spacesN
  cs3 <- pComments
  tExpr <- pTypeExpression 
  pSemic >> spacesL
  cs4 <- pCommentOptional
  return (TypeDeclaration id tExpr, [cs1, cs2, cs3, cs4])


-- TypeExpression ------------------------

pTypeExpression :: Parser (Commented TypeExpression)
pTypeExpression = try pArrayTypeExpression <|> pNamedTypeExpression

pNamedTypeExpression :: Parser (Commented TypeExpression)
pNamedTypeExpression = do
  id <- pIdent << spacesN
  cs <- pComments 
  return (NamedTypeExpression id, [cs])

pArrayTypeExpression :: Parser (Commented TypeExpression)
pArrayTypeExpression = do
  pArray >> spacesN
  cs1 <- pComments
  pLBrack >> spacesN
  cs2 <- pComments
  idx <- pIntLit << spacesN
  cs3 <- pComments
  pRBrack >> spacesN
  cs4 <- pComments
  pOf >> spacesN
  cs5 <- pComments
  tExpr <- pTypeExpression
  return (ArrayTypeExpression idx tExpr, [cs1, cs2, cs3, cs4, cs5])


-- Variables -----------------------------

pVariable :: Parser (Commented Variable)
pVariable =  try pArrayAccess <|> pNamedVariable

pNamedVariable :: Parser (Commented Variable)
pNamedVariable = do
  id <- pIdent << spacesN
  cs <- pComments
  return (NamedVariable id, [cs])

pArrayAccess :: Parser (Commented Variable)
pArrayAccess = do 
  nVar <- pNamedVariable
  exprs <- many1 pAccess
  let acc = foldl (\v expr@(e, css) -> (ArrayAccess v expr, css)) nVar exprs
  return acc

pAccess :: Parser (Commented Expression)
pAccess = do 
  pLBrack >> spacesN
  cs1 <- pComments 
  (expr, css) <- pExpression
  pRBrack >> spacesN
  cs2 <- try (pComments <|> return [])
  return (expr, [cs1] ++ css ++ [cs2])


-- Procedure Declaration -----------------

pProcedureDeclaration :: Parser (Commented GlobalDeclaration)
pProcedureDeclaration = do
  pProc >> spacesN
  cs1 <- pComments
  id <- pIdent << spacesN
  cs2 <- pComments
  pLParen >> spacesN
  cs3 <- pComments 
  pDecs <- option [] pParameterDeclarationList
  pRParen >> spacesN
  cs4 <- pComments
  pLCurl >> spacesL
  cs5 <- pCommentOptional
  vDecs <- many pVariableDeclaration
  stmts <- many pStatement
  pRCurl >> spacesL
  cs6 <- pCommentOptional
  return (ProcedureDeclaration id pDecs vDecs stmts, [cs1, cs2, cs3, cs4, cs5, cs6])


-- Parameter Declaration -----------------

pParameterDeclarationList :: Parser ([Commented ParameterDeclaration])
pParameterDeclarationList = do
  dec <- pParameterDeclaration
  decs <-  many $ do
    pComma >> spacesN
    cs <- pComments
    (dec, css) <- pParameterDeclaration
    return (dec, [cs] ++ css)
  cs2 <- pComments
  return (dec:decs)

pParameterDeclaration :: Parser (Commented ParameterDeclaration)
pParameterDeclaration = do
  ref <- option False (do {try pRef; return True}) << spacesN
  cs1 <- pComments
  id <- pIdent << spacesN
  cs2 <- pComments
  pColon >> spacesN
  cs3 <- pComments
  tExpr <- pTypeExpression
  return (ParameterDeclaration id tExpr ref, [cs1, cs2, cs3])
  

-- Variable Declaration ------------------

pVariableDeclaration :: Parser (Commented VariableDeclaration)
pVariableDeclaration = try pVariableDeclaration_ <|> pVariableDeclarationComment

pVariableDeclaration_ :: Parser (Commented VariableDeclaration)
pVariableDeclaration_ = do
  pVar >> spacesN
  cs1 <- pComments 
  id <- pIdent << spacesN
  cs2 <- pComments 
  pColon >> spacesN
  cs3 <- pComments
  tExpr <- pTypeExpression 
  pSemic >> spacesL
  cs4 <- pCommentOptional
  return (VariableDeclaration id tExpr, [cs1, cs2, cs3, cs4])

pVariableDeclarationComment :: Parser (Commented VariableDeclaration)
pVariableDeclarationComment = do 
  c <- pComment
  return (VariableDeclarationComment c, [])


-- Statements ----------------------------

pStatement :: Parser (Commented Statement)
pStatement = try pWhileStatement 
         <|> pStatementEmptyLine
         <|> try pAssignStatement 
         <|> pCompoundStatement 
         <|> pEmptyStatement 
         <|> pStatementComment 
         <|> try pIfStatement 
         <|> pCallStatement

pAssignStatement :: Parser (Commented Statement)
pAssignStatement = do
  id <- pVariable
  pASGN >> spacesN
  cs1 <- pComments
  tExpr <- pExpression 
  pSemic >> spacesL
  cs2 <- pCommentOptional
  return (AssignStatement id tExpr, [cs1, cs2])

pWhileStatement :: Parser (Commented Statement)
pWhileStatement = do
  pWhile >> spacesN
  cs1 <- pComments
  pLParen >> spacesN
  cs2 <- pComments 
  expr <- pExpression 
  pRParen >> spacesN
  cs3 <- pComments
  stmt <- pStatement
  return (WhileStatement expr stmt, [cs1, cs2, cs3])

pCompoundStatement :: Parser (Commented Statement)
pCompoundStatement = do
  pLCurl  >> spacesL
  cs1 <- pCommentOptional
  stmt <- many pStatement 
  pRCurl >> spacesL
  cs2 <- pCommentOptional
  return (CompoundStatement stmt, [cs1, cs2])

pEmptyStatement :: Parser (Commented Statement) 
pEmptyStatement = do 
  pSemic  >> spacesL
  cs1 <- pCommentOptional
  return $ (EmptyStatement, [cs1])

pStatementComment :: Parser (Commented Statement)
pStatementComment = do 
  c <- pComment
  return (StatementComment c, [])

pStatementEmptyLine :: Parser (Commented Statement)
pStatementEmptyLine = endOfLine >> spacesL >> return (StatementEmptyLine, [])

pIfStatement :: Parser (Commented Statement)
pIfStatement = do
  pIf >> spacesN
  cs1 <- pComments
  pLParen >> spacesN
  cs2 <- pComments 
  expr <- pExpression 
  pRParen >> spacesN
  cs3 <- pComments
  stmt <- pStatement
  optElse <- optionMaybe $ try (spacesN >> pComments >> pElseStatement)
  let (optstmt, cs4) = case optElse of 
                  Nothing    -> (Nothing, [])
                  Just (stmt2, cs) -> (Just stmt2, cs)
  return (IfStatement expr stmt optstmt, [cs1, cs2, cs3, cs4])

pElseStatement  :: Parser ((Commented Statement), [Comment])
pElseStatement = do 
  pElse >> spacesN
  cs1 <- pComments
  stmt <- pStatement
  return (stmt, cs1)

pCallStatement :: Parser (Commented Statement)
pCallStatement = do 
  id <- pIdent << spacesN
  cs1 <- pComments 
  pLParen >> spacesN
  cs2 <- pComments
  exprs <- option [] pExpressionList
  pRParen >> spacesN
  cs3 <- pComments
  pSemic >> spacesL
  cs4 <- pCommentOptional
  return (CallStatement id exprs, [cs1, cs2, cs3, cs4])

pExpressionList :: Parser ([Commented Expression])
pExpressionList = do 
  expr <- pExpression 
  exprs <- many $ do 
    pComma >> spacesN
    cs <- pComments 
    (expr, css) <- pExpression
    return (expr, [cs] ++ css)
  return (expr:exprs)


-- *** Expressions
------------------------------------------

-- Expression ----------------------------

pExpression :: Parser (Commented Expression)
pExpression = buildExpressionParser table term 

term :: Parser (Commented Expression)
term = pParenthesized <|> pVariableExpression <|> pIntLiteral

pParenthesized :: Parser (Commented Expression)
pParenthesized = do
  pLParen >> spaces 
  cs1 <- pComments
  expr <- pExpression
  pRParen >> spaces 
  cs2 <- pComments
  return (Parenthesized expr, [cs1, cs2])

table   = [ 
            [prefix pMINUS neg, prefix pPLUS pos],
            [binary pOpPoint],
            [binary pOpDash],
            [binary pOpCompare]
          ]

pVariableExpression ::  Parser (Commented Expression)
pVariableExpression = do
  (id, css) <- pVariable
  return (VariableExpression (id, css), css)

pIntLiteral :: Parser (Commented Expression)
pIntLiteral = do
  intLit <- pIntLit << spacesN
  cs1 <- pComments
  return (IntLiteral intLit, [cs1])


-- Expression Utilities ------------------

getBiExpr :: Commented Op -> Commented Expression -> Commented Expression -> Commented Expression
getBiExpr op expr1@(e1, css1) expr2@(e2, css2) = (BinaryExpression op expr1 expr2, css1 ++  css2)

getPreExpr :: [[Comment]] -> (Commented Expression -> Commented Expression) -> Commented Expression -> Commented Expression
getPreExpr css1 f (e, css2) = f (e, css1 ++ css2)  

pBiOperator :: Parser Op -> Parser (Commented Expression -> Commented Expression -> Commented Expression)
pBiOperator pOp = do
  op <- pOp << spacesN
  cs <- pComments
  return (getBiExpr (op, [cs]))

pPreOperator :: (Commented Expression -> Commented Expression) -> Parser Op -> Parser ((Commented Expression) -> (Commented Expression))
pPreOperator f pOp = do
  pOp >> spacesN
  cs <- pComments
  return $ getPreExpr [cs] f

binary pOp = Infix (pBiOperator pOp) AssocLeft
prefix pOp f = Prefix  (pPreOperator f pOp)  

neg :: Commented Expression -> Commented Expression
neg (e, css) = (Negative (e, tail css), [head css])

pos :: Commented Expression -> Commented Expression
pos (e, css) = (Positive (e, tail css), [head css])


---------------------------------------------------------
-- * Utility functions
---------------------------------------------------------

-- |Consumes a sequence of any spaces except for newlines.
spacesL :: Parser () 
spacesL = many (char ' ' <|> char '\t') >> return ()

-- |Consumes a sequence of any spaces including newlines.
spacesN :: Parser () 
spacesN = spaces

-- |Execute two monadic actions and ignore the result of the second one.
(<<) :: Monad m => m a -> m b -> m a
f << g = do 
  x <- f 
  g 
  return x