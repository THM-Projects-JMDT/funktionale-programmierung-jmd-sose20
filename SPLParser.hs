module SPLParser where

import Text.Parsec
import Data.Char

{-
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A parser for SPL - based on parsec-combinators
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-}


type Parser a = Parsec String () a -- for convenience


-- utility -------------------------------

-- builds a parser that consumes following spaces
token_ :: Parser a -> Parser a 
token_ p = do
  x <- p 
  spaces
  return x


-- comma, semicolon, colon ---------------

pComma  = token_ $ char ','       :: Parser Char
pSemic  = token_ $ char ';'       :: Parser Char
pColon  = token_ $ char ':'       :: Parser Char


-- keywords ------------------------------

pElse  = token_ $ string "else"   :: Parser String
pWhile = token_ $ string "while"  :: Parser String
pRef   = token_ $ string "ref"    :: Parser String
pIf    = token_ $ string "if"     :: Parser String
pOf    = token_ $ string "of"     :: Parser String
pType  = token_ $ string "type"   :: Parser String
pProc  = token_ $ string "proc"   :: Parser String
pArray = token_ $ string "array"  :: Parser String
pVar   = token_ $ string "var"    :: Parser String


-- brackets, parantheses -----------------

pLParen = token_ $ char '('       :: Parser Char
pRParen = token_ $ char ')'       :: Parser Char
pLCurl  = token_ $ char '{'       :: Parser Char
pRCurl  = token_ $ char '}'       :: Parser Char
pLBrack = token_ $ char '['       :: Parser Char
pRBrack = token_ $ char ']'       :: Parser Char


-- operators

data Op = Lt | Ne | Asgn | Plus | Slash | Star | Gt | Le | Minus | Ge | Eq 
          deriving (Eq, Show) 

pLT    = token_ $ char '<' >> return Lt       :: Parser Op
pNE    = token_ $ char '#' >> return Ne       :: Parser Op
pASGN  = token_ $ string ":=" >> return Asgn  :: Parser Op
pPLUS  = token_ $ char '+' >> return Plus     :: Parser Op
pSLASH = token_ $ char '/' >> return Slash    :: Parser Op
pSTAR  = token_ $ char '*' >> return Star     :: Parser Op
pGT    = token_ $ char '>' >> return Gt       :: Parser Op
pLE    = token_ $ string "<=" >> return Le    :: Parser Op
pMINUS = token_ $ char '-' >> return Minus    :: Parser Op
pGE    = token_ $ string ">=" >> return Ge    :: Parser Op
pEQ    = token_ $ char '=' >> return Eq       :: Parser Op


-- integer literals ----------------------

pIntLit :: Parser Int
pIntLit = token_ $ pHexLit 
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
 
 -- TODO
