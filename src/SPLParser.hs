module SPLParser where

import Text.Parsec
import Text.Parsec.Expr
import Data.Char

import SPLAbsyn

{-
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A parser for SPL - based on parsec-combinators
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-}


type Parser a = Parsec String () a -- for convenience



-- utility ---------------------------------------------------
--------------------------------------------------------------

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



-- Token Parsers ---------------------------------------------
--------------------------------------------------------------

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


-- comments ------------------------------

-- parses a single comment and following spaces in the next line (e.g. "// this is a comment \n   ")
pComment :: Parser Comment 
pComment = try $ do 
  string "//"
  cs <- manyTill anyChar (try endOfLine)
  spacesL
  return cs

-- parses an optional comment and returns an empty list (if there is no comment) or a singleton (if there is one)
pCommentOptional :: Parser [Comment]
pCommentOptional = do 
  la <- lookAhead $ optionMaybe anyChar
  cm <- case la of 
             Just '\n' -> newline >> spacesL >> return Nothing
             Just '/'  -> Just <$> pComment
             otherwise -> return Nothing
  let cs = case cm of 
             Nothing -> []
             Just c  -> [c]
  return cs

-- parses a sequence of comments with optional spaces inbetween and following spaces (e.g. "// comment A \n\n // comment B  \n")
pComments :: Parser [Comment]
pComments = many $ pComment << spacesN



-- SPL-Grammar -----------------------------------------------
--------------------------------------------------------------

-- Program -------------------------------

pProgram :: Parser [Commented GlobalDeclaration]
pProgram = many $ pGlobalEmptyLine <|> pGlobalComment <|> pTypeDeclaration <|> pProcedureDeclaration


-- Global Empty Line --------------------

pGlobalEmptyLine :: Parser (Commented GlobalDeclaration)
pGlobalEmptyLine = newline >> spacesL >> return (GlobalEmptyLine, [])


-- Global Comment

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
pTypeExpression = pArrayTypeExpression <|> pNamedTypeExpression

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
  let acc = foldl (\v e -> (ArrayAccess v e, [])) nVar exprs
  return acc

pAccess :: Parser (Commented Expression)
pAccess = do 
  pLBrack >> spacesN
  cs1 <- pComments 
  (expr, css) <- pExpression
  pRBrack >> spacesN
  cs2 <- try (pComments <|> return [])
  return (expr, [cs1] ++ css ++ [cs2])


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
  id <- pVariable
  return (VariableExpression id, [])

pIntLiteral :: Parser (Commented Expression)
pIntLiteral = do
  intLit <- pIntLit << spacesN
  cs1 <- pComments
  return (IntLiteral intLit, [cs1])


-- Expression Utilities ------------------

getBiExpr :: Commented Op -> Commented Expression -> Commented Expression -> Commented Expression
getBiExpr op e1 e2 = (BinaryExpression op e1 e2, [])

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
  pLCurl >> spacesN
  cs5 <- pComments
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
pVariableDeclaration = do
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
  cs1 <- pComments 
  pASGN >> spacesN
  cs2 <- pComments
  tExpr <- pExpression 
  pSemic >> spacesL
  cs3 <- pCommentOptional
  return (AssignStatement id tExpr, [cs1, cs2, cs3])

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
  pRCurl >> spacesN
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
pStatementEmptyLine = newline >> spacesL >> return (StatementEmptyLine, [])

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
  optElse <- try (optionMaybe $ spacesN >> pComments >> pElseStatement)
  let optstmt = case optElse of 
                  Nothing    -> Nothing
                  Just stmt2 -> Just stmt2
  return (IfStatement expr stmt optstmt, [cs1, cs2, cs3])

pElseStatement  :: Parser (Commented Statement)
pElseStatement = do 
  pElse >> spacesN
  cs1 <- pComments
  (stmt, css) <- pStatement
  return (stmt, [cs1] ++ css)

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
