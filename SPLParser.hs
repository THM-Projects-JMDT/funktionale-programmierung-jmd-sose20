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

pComma, pSemic, pColon :: Parser Char
pComma  = token_ $ char ','   
pSemic  = token_ $ char ';'    
pColon  = token_ $ char ':'    


-- keywords ------------------------------

pElse, pWhile, pRef, pIf, pOf, pType, pProc, pArray, pVar :: Parser String
pElse  = token_ $ string "else"   
pWhile = token_ $ string "while"  
pRef   = token_ $ string "ref"    
pIf    = token_ $ string "if"  
pOf    = token_ $ string "of"     
pType  = token_ $ string "type"   
pProc  = token_ $ string "proc"   
pArray = token_ $ string "array"  
pVar   = token_ $ string "var"   


-- brackets, parantheses -----------------

pLParen, pRParen, pLCurl, pRCurl, pLBrack, pRBrack :: Parser Char
pLParen = token_ $ char '('       
pRParen = token_ $ char ')'      
pLCurl  = token_ $ char '{'      
pRCurl  = token_ $ char '}'      
pLBrack = token_ $ char '['      
pRBrack = token_ $ char ']'      


-- operators

data Op = Lt | Ne | Asgn | Plus | Slash | Star | Gt | Le | Minus | Ge | Eq 
          deriving (Eq, Show) 

pLT, pNE, pASGN, pPLUS, pSLASH, pSTAR, pGT, pLE, pMINUS, pGE, pEQ :: Parser Op
pLT    = token_ $ char '<' >> return Lt   
pNE    = token_ $ char '#' >> return Ne       
pASGN  = token_ $ string ":=" >> return Asgn  
pPLUS  = token_ $ char '+' >> return Plus   
pSLASH = token_ $ char '/' >> return Slash   
pSTAR  = token_ $ char '*' >> return Star    
pGT    = token_ $ char '>' >> return Gt   
pLE    = token_ $ string "<=" >> return Le  
pMINUS = token_ $ char '-' >> return Minus   
pGE    = token_ $ string ">=" >> return Ge  
pEQ    = token_ $ char '=' >> return Eq   


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
 
pIdent :: Parser String
pIdent = token_ $ do
  x <- upper <|> lower <|> char '_'
  xs <- many $ alphaNum <|> char '_'
  return (x : xs)