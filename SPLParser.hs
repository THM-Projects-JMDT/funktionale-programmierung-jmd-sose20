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

pComma_ = token_ $ char ','       :: Parser Char
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

-- TODO


-- operators

data Op = Lt | Ne | Asgn | Plus | Slash | Star | Gt | Le | Minus | Ge | Eq 
          deriving (Eq, Show) 

pLT = char '<' >> return Lt  :: Parser Op
pNE = char '#' >> return Ne  :: Parser Op
pASGN = string ":=" >> return Asgn  :: Parser Op
pPLUS = char '+' >> return Plus  :: Parser Op
pSLASH = char '/' >> return Slash  :: Parser Op
pSTAR = char '*' >> return Star  :: Parser Op
pGT = char '>' >> return Gt  :: Parser Op
pLE = string "<=" >> return Le  :: Parser Op
pMINUS = char '-' >> return Minus  :: Parser Op
pGE = string ">=" >> return Ge  :: Parser Op
pEQ = char '=' >> return Eq  :: Parser Op
-- TODO


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
