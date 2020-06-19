module SPLParser where

import Text.Parsec
import Data.Char

import SPLAbsyn

{-
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A parser for SPL - based on parsec-combinators
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-}


type Parser a = Parsec String () a -- for convenience


-- utility -------------------------------

-- consumes a sequence of any spaces except for newlines (=> only spaces in the same line)
spacesL :: Parser () 
spacesL = many (char ' ' <|> char '\t') >> return ()

-- consumes a sequence of any spaces including newlines
spacesN :: Parser () 
spacesN = spaces

-- returns a parser that consumes following spaces in the same line
wsL :: Parser a -> Parser a
wsL p = p << spacesL

-- returns a parser that consumes all following spaces
wsN :: Parser a -> Parser a 
wsN p = p << spacesN

-- execute two actions and ignore the result of the second one
(<<) :: Monad m => m a -> m b -> m a
f << g = do 
  x <- f 
  g 
  return x


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


-- operators

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


-- integer literals ----------------------

pIntLit :: Parser Int
pIntLit = pHexLit 
               <|> pCharLit 
               <|> pDecLit

pHexLit = do 
  x  <- string "0x"
  xs <- many1 hexDigit
  return $ read (x ++ xs)

pCharLit = do 
  n <- between (char '\'') (char '\'') anyChar
  return $ ord n

pDecLit = do 
  ds <- many1 digit 
  return $ read ds


-- identifiers ---------------------------
 
pIdent :: Parser String
pIdent = do
  x <- upper <|> lower <|> char '_'
  xs <- many $ alphaNum <|> char '_'
  return (x : xs)


-- comments ------------------------------

-- parses a single comment and following spaces (e.g. "// this is a comment \n   ")
pComment :: Parser Comment 
pComment = do 
  string "//"
  cs <- manyTill anyChar (try endOfLine)
  spacesN
  return cs

-- parses a single comment - if there is one - and following spaces 
pCommentOptional :: Parser [Comment]
pCommentOptional = do 
  cm <- optionMaybe pComment
  spacesN 
  let cs = case cm of 
             Nothing -> []
             Just c  -> [c]
  return cs

-- parses a sequence of comments with optional spaces inbetween and following spaces (e.g. "// comment A \n\n // comment B  \n")
pComments :: Parser [Comment]
pComments = do 
  cs <- many $ pComment
  return cs


-- SPL-Grammar ---------------------------

-- Programm ---------------------------

-- Global Declarion ---------------------------

-- TypeDeclaration -----------------------

pTypeDeclaration :: Parser GlobalDeclaration
pTypeDeclaration = do 
  pType >> spacesN
  cs1 <- pComments 
  id <- pIdent << spacesN
  cs2 <- pComments 
  pEQ >> spacesN
  cs3 <- pComments
  (tExpr, cs4) <- pTypeExpression 
  pSemic >> spacesL
  cs5 <- pCommentOptional
  return $ TypeDeclaration id tExpr (cs1 ++ cs2 ++ cs3 ++ cs4 ++ cs5)

-- TypeExpression ---------------------------

pTypeExpression :: Parser (TypeExpression, [Comment])
pTypeExpression = pArrayTypeExpression <|> pNamedTypeExpression

pNamedTypeExpression :: Parser (TypeExpression, [Comment])
pNamedTypeExpression = do
  id <- pIdent << spacesN
  cs <- pComments 
  return $ (NamedTypeExpression id, cs)

pArrayTypeExpression :: Parser (TypeExpression, [Comment])
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
  (tExpr, cs6) <- pTypeExpression
  cs7 <- pComments
  return $ (ArrayTypeExpression idx tExpr, cs1 ++ cs2 ++ cs3 ++ cs4 ++ cs5 ++ cs6 ++ cs7)


-- Expression ---------------------------

pExpression:: Parser (Expression, [Comment])
pExpression = pVariableExpression -- <|> IntLiteral <|> BinaryExpression ( => TODO)

pVariableExpression ::  Parser (Expression, [Comment])
pVariableExpression = do
  (id, cs1) <- pVariable << spacesN
  cs2 <- pComments 
  return $ (VariableExpression id, (cs1 ++ cs2))


-- Variables ---------------------------

pVariable :: Parser (Variable, [Comment])
pVariable = pNamedVariable -- <|> ArrayAccess ( => TODO)

pNamedVariable :: Parser (Variable, [Comment])
pNamedVariable = do
  id <- pIdent << spacesN
  cs <- pComments 
  return $ (NamedVariable id, cs)


-- ParameterDeclaration ---------------------------

pParameterDeclarations :: Parser ([ParameterDeclaration], [Comment])
pParameterDeclarations = do 
  pLParen >> spacesN
  cs1 <- pComments
  (dec, cs2) <- option ([], []) pParameterDeclarationList
  pRParen >> spacesN
  cs3 <- pComments
  return (dec, cs1 ++ cs2 ++ cs3)

pParameterDeclarationList :: Parser ([ParameterDeclaration], [Comment])
pParameterDeclarationList = do
  (dec, cs1) <- pParameterDeclaration
  decs <-  many (pComma >> spacesN >> pParameterDeclaration) << spacesN
  cs2 <- pComments
  return (dec : map fst decs, cs1 ++ concatMap  snd decs ++ cs2)

pParameterDeclaration :: Parser (ParameterDeclaration, [Comment])
pParameterDeclaration = do
  ref <- option False (do {pRef; return True}) << spacesN
  cs1 <- pComments
  id <- pIdent << spacesN
  cs2 <- pComments
  pColon >> spacesN
  cs3 <- pComments
  (tExpr, cs4) <- pTypeExpression
  cs5 <- pComments
  return (ParameterDeclaration id tExpr ref, cs1 ++ cs2 ++ cs3 ++ cs4 ++ cs5)


-- Variable Declarion ---------------------------

pVariableDeclaration :: Parser (VariableDeclaration)
pVariableDeclaration = do
  pVar >> spacesN
  cs1 <- pComments 
  id <- pIdent << spacesN
  cs2 <- pComments 
  pColon >> spacesN
  cs3 <- pComments
  (tExpr, cs4) <- pTypeExpression 
  pSemic >> spacesL
  cs5 <- pCommentOptional
  return $ VariableDeclaration id tExpr (cs1 ++ cs2 ++ cs3 ++ cs4 ++ cs5)

-- Statements ---------------------------

pStatement :: Parser (Statement)
pStatement = pWhileStatement <|> pAssignStatement-- <|> CallStatement <|> CompoundStatement <|> EmptyStatement <|> IfStatement <|> StatementComment ( => TODO)

pAssignStatement :: Parser (Statement)
pAssignStatement = do
  (id, cs1) <- pVariable << spacesN
  cs2 <- pComments 
  pASGN >> spacesN
  cs3 <- pComments
  (tExpr, cs4) <- pExpression 
  pSemic >> spacesL
  cs5 <- pCommentOptional
  return $ AssignStatement id tExpr (cs1 ++ cs2 ++ cs3 ++ cs4 ++ cs5)

pWhileStatement :: Parser (Statement)
pWhileStatement = do
  pWhile >> spacesN
  cs1 <- pComments
  pLParen >> spacesN
  cs2 <- pComments 
  (tExpr, cs3) <- pExpression 
  pRParen >> spacesN
  cs5 <- pComments
  stmt <- pStatement
  cs6 <- pCommentOptional
  return $ WhileStatement tExpr stmt (cs1 ++ cs2 ++ cs3 ++ cs5 ++ cs6)



-- test example
tDecl = "while (hallo) while (hallo) i := hallo;"