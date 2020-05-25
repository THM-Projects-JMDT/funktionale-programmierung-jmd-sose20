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

keyword :: String -> Parser ()
keyword s = string s >> spaces

symbol :: Char -> Parser ()
symbol c = char c >> spaces


-- comma, semicolon, colon ---------------

pComma_ = symbol ','           :: Parser ()
pSemic  = symbol ';'           :: Parser ()
pColon  = symbol ':'           :: Parser ()


-- keywords ------------------------------

pElse  = keyword "else"        :: Parser ()
pWhile = keyword "while"       :: Parser ()
pRef   = keyword "ref"         :: Parser ()
pIf    = keyword "if"          :: Parser ()
pOf    = keyword "of"          :: Parser ()
pType  = keyword "type"        :: Parser ()
pProc  = keyword "proc"        :: Parser ()
pArray = keyword "array"       :: Parser ()
pVar   = keyword "var"         :: Parser ()


-- brackets, parantheses -----------------

-- TODO


-- operators

data Op = Lt | Ne | Asgn | Plus | Slash | Star | Gt | Le | Minus | Ge | Eq 
          deriving (Eq, Show) 

pLT = char '<' >> return Lt  :: Parser Op
-- TODO


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
 
 -- TODO
