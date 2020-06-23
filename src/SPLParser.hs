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

pBiOperator :: Parser Op
pBiOperator = try pLE <|> try pGE <|> pLT <|> pNE <|> pPLUS <|> pSLASH <|>  pSTAR <|> pGT <|> pMINUS <|> pEQ

-- integer literals ----------------------

pIntLit :: Parser IntString
pIntLit = try pHexLit 
          <|> pCharLit 
          <|> pDecLit

pHexLit = do 
  x  <- string "0x"
  xs <- many1 hexDigit
  return $ x ++ xs

pCharLit = do 
  n <- between (char '\'') (char '\'') anyChar
  return $ "'" ++ [n] ++ "'"

pDecLit = do 
  ds <- many1 digit 
  return $ ds


-- identifiers ---------------------------
 
pIdent :: Parser String
pIdent = do
  x <- upper <|> lower <|> char '_'
  xs <- many $ alphaNum <|> char '_'
  return (x : xs)


-- comments ------------------------------

-- parses a single comment and following spaces in the next line (e.g. "// this is a comment \n   ")
pComment :: Parser Comment 
pComment = do 
  string "//"
  cs <- manyTill anyChar (try endOfLine)
  spacesL
  return cs

-- parses a single comment - if there is one - and following spaces in the next line
pCommentOptional :: Parser [Comment]
pCommentOptional = do 
  la <- lookAhead $ optionMaybe anyChar
  cm <- case la of 
             Nothing   -> return Nothing
             Just '/'  -> Just <$> pComment
             otherwise -> newline >> spacesL >> return Nothing
  let cs = case cm of 
             Nothing -> []
             Just c  -> [c]
  return cs

-- parses a sequence of comments with optional spaces inbetween and following spaces (e.g. "// comment A \n\n // comment B  \n")
pComments :: Parser [Comment]
pComments = many $ pComment << spacesN


-- SPL-Grammar ---------------------------

-- Program -------------------------------

pProgram :: Parser [GlobalDeclaration]
pProgram = many $ pGlobalEmptyLine <|> pGlobalComment <|> pTypeDeclaration {-- <|> pProcedureDeclaration --}


-- Global Empty Line --------------------

pGlobalEmptyLine :: Parser GlobalDeclaration
pGlobalEmptyLine = newline >> spacesL >> return GlobalEmptyLine


-- Global Comment

pGlobalComment :: Parser GlobalDeclaration
pGlobalComment = GlobalComment <$> pComment


-- Type Declaration -----------------------

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


-- TypeExpression ------------------------

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


-- Expression Utilities ------------------

-- TODO: comments after the operator do not work yet 
binary s o = Infix (string s >> spacesN >> return (getBinExpr o)) AssocLeft
prefix s f = Prefix  (string s >> spacesN >> return f)  

-- TODO: may have to be solved differently to distinguish between '0 - expr' and '- expr'  
neg :: (Expression, [Comment]) -> (Expression, [Comment])
neg (e, c) = (BinaryExpression Minus (IntLiteral "0") e, c)

getBinExpr :: Op -> (Expression, [Comment]) -> (Expression, [Comment]) -> (Expression, [Comment])
getBinExpr o e1 e2 = (BinaryExpression o (fst e1) (fst e2), snd e1 ++ snd e2)

-- Expression ----------------------------

pExpression :: Parser (Expression, [Comment])
pExpression    = buildExpressionParser table term

term :: Parser (Expression, [Comment])
-- TODO: If a parenthesized expression is at the beginning, the rest of the expression is ignored 
term = between pLParen pRParen pExpression <|> pVariableExpression <|> pIntLiteral

table   = [ 
            [prefix "-" neg, prefix "+" id],
            [binary "*" Star, binary "/" Slash],
            [binary "+" Plus, binary "-" Minus],
            -- TODO: <= and >= are not working => unexpected "="
            [binary "<" Lt, binary ">" Gt, binary "<=" Le, binary ">=" Ge, binary "#" Ne, binary "=" Eq]
          ]

pVariableExpression ::  Parser (Expression, [Comment])
pVariableExpression = do
  (id, cs1) <- pVariable << spacesN
  cs2 <- pComments 
  return $ (VariableExpression id, (cs1 ++ cs2))

pIntLiteral :: Parser (Expression, [Comment])
pIntLiteral = do
  inlit <- pIntLit << spacesN
  cs1 <- pComments
  return $ (IntLiteral inlit, cs1)

-- Variables -----------------------------

pVariable :: Parser (Variable, [Comment])
pVariable =  try pArrayAccess <|> pNamedVariable

pNamedVariable :: Parser (Variable, [Comment])
pNamedVariable = do
  id <- pIdent << spacesN
  cs <- pComments 
  return $ (NamedVariable id, cs)

pArrayAccess :: Parser (Variable, [Comment])
pArrayAccess = do 
  (nVar, cs1) <- pNamedVariable
  cs2 <- pComments
  (exprs, css) <- unzip <$> many1 pAccess
  let acc = foldl ArrayAccess nVar exprs
  return (acc, cs1 ++ cs2 ++ concat css)

pAccess :: Parser (Expression, [Comment])
pAccess = do 
  pLBrack >> spacesN
  cs1 <- pComments 
  (expr, cs2) <- pExpression
  pRBrack >> spacesN
  cs3 <- pComments
  return (expr, cs1 ++ cs2 ++ cs3)

-- Parameter Declaration -----------------

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


-- Variable Declaration ------------------

pVariableDeclaration :: Parser VariableDeclaration
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


-- Statements ----------------------------

pStatement :: Parser Statement
pStatement = pWhileStatement 
         <|> pAssignStatement 
         <|> pCompoundStatement 
         <|> pEmptyStatement 
         <|> pStatementComment 
         <|> pIfStatement 
         <|> pCallStatement

pAssignStatement :: Parser Statement
pAssignStatement = do
  (id, cs1) <- pVariable << spacesN
  cs2 <- pComments 
  pASGN >> spacesN
  cs3 <- pComments
  (tExpr, cs4) <- pExpression 
  pSemic >> spacesL
  cs5 <- pCommentOptional
  return $ AssignStatement id tExpr (cs1 ++ cs2 ++ cs3 ++ cs4 ++ cs5)

pWhileStatement :: Parser Statement
pWhileStatement = do
  pWhile >> spacesN
  cs1 <- pComments
  pLParen >> spacesN
  cs2 <- pComments 
  (expr, cs3) <- pExpression 
  pRParen >> spacesN
  cs5 <- pComments
  stmt <- pStatement
  return $ WhileStatement expr stmt (cs1 ++ cs2 ++ cs3 ++ cs5)

pCompoundStatement :: Parser Statement
pCompoundStatement = do
  pLCurl  >> spacesN
  cs1 <- pComments
  stmt <- many pStatement << spacesN
  pRCurl >> spacesN
  cs3 <- pCommentOptional
  return $ CompoundStatement stmt (cs1 ++cs3)

pEmptyStatement :: Parser Statement 
pEmptyStatement = do 
  pSemic  >> spacesL
  cs1 <- pCommentOptional
  return $ EmptyStatement cs1

pStatementComment :: Parser Statement
pStatementComment = StatementComment <$> pComment

pIfStatement :: Parser Statement
pIfStatement = do
  pIf >> spacesN
  cs1 <- pComments
  pLParen >> spacesN
  cs2 <- pComments 
  (expr, cs3) <- pExpression 
  pRParen >> spacesN
  cs4 <- pComments
  stmt <- pStatement
  optElse <- optionMaybe pElseStatement
  let (optstmt, cs5) = case optElse of 
                      Nothing             -> (Nothing, [])
                      Just (stmt2, optcs) -> (Just stmt2, optcs)
  return $ IfStatement expr stmt optstmt(cs1 ++ cs2 ++ cs3 ++ cs4 ++ cs5)

pElseStatement  :: Parser (Statement, [Comment])
pElseStatement = do 
  pElse >> spacesN
  cs1 <- pComments
  stmt <- pStatement
  return $ (stmt, cs1)

pCallStatement :: Parser Statement
pCallStatement = do 
  id <- pIdent << spacesN
  cs2 <- pComments 
  pLParen >> spacesN
  cs3 <- pComments
  (expr, cs4) <- pExpression
  exprs <-  many (pComma >> spacesN >> pExpression << spacesN)
  cs5 <- pComments
  pRParen >> spacesN
  cs6 <- pComments
  return $ CallStatement id (expr : map fst exprs) (cs2 ++ cs3 ++ cs4 ++ concatMap snd exprs ++ cs5 ++cs6)

-- test example
tDecl = "hallo(10,// hallo \n20,30)"

tAcc = "arr\n  \n//A\n[//B\n \n 4\n//C\n ]//D\n\n[5] //E\n//F\n"