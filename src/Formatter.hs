{-|
Module      : Formatter
Description : Pretty Printer for the SPL-AST.

This module provides functions for converting an SPL-AST structure into a pretty printed string.
-}

module Formatter where

import           Absyn
import           Text.Parsec
import           Parser


-- |This type encapsulates the different configuration options for the pretty printing functions.
data Config = Config {
    indentType :: IndentationType,
    indentNum :: Int,
    removeUnnecessarySigns :: Bool,
    keepAllComments :: Bool,
    newlineEncoding :: NewlineEncoding
} deriving (Eq, Show)

-- |An indentation can be a sequence of spaces or a sequence of tabs 
-- - this type encapsulates these two options.
data IndentationType = Space
                     | Tab
                     deriving (Eq, Show)

-- |Different operating systems use different new line character encodings 
-- ('\n', '\r\n', '\r') 
-- - this type encapsulates the different options.
data NewlineEncoding = Linux
                  | Windows
                  | ClassicMac
                  deriving (Eq, Show)

-- |Base type alias for the pretty printing functions.
--
-- A PrettyPrinter takes as arguments a Config and an IndentationLevel 
-- and returns a pretty printed string. 
type PrettyPrinter a = Config -> IndentationLevel -> a -> String

-- |The indentation level determines the number of leading spaces/tabs of a 
-- pretty printed string.
type IndentationLevel = Int



-- for testing -----------------------------------------------
--------------------------------------------------------------

defaultConfig = Config Space 2 True True Linux

run2 :: Parser a -> String -> Either ParseError a
run2 p = runParser p () ""

testFormat2 :: String -> Parser a -> PrettyPrinter a -> IO ()
testFormat2 s p f = putStrLn $ case run2 p s of
  Left  err -> "Parser failed: " ++ show err
  Right r   -> f defaultConfig 2 r



--------------------------------------------------------------
-- * Pretty Printers
--------------------------------------------------------------


-- ** Comments
-----------------------------------------------------

-- |Returns a pretty printed comment.
fComment :: PrettyPrinter Comment
fComment conf _ c = "//" ++ c ++ newline_ conf

-- |Returns an indented pretty printed comment.
fLineComment :: PrettyPrinter Comment
fLineComment conf c cm = indent conf c ++ fComment conf c cm

-- |Returns a pretty printed comment with one leading space.
_fComment :: PrettyPrinter Comment
_fComment conf c cm = " " ++ fComment conf c cm

-- |Returns a pretty printed comment or a space.
fOptionalComment :: PrettyPrinter [Comment]
fOptionalComment conf c cs =
  if null cs then " " else _fComment conf c (head cs)

-- |Returns a pretty printed comment or a new line.
fOptionalComment_ :: PrettyPrinter [Comment]
fOptionalComment_ conf c cs =
  if null cs then newline_ conf else _fComment conf c (head cs)

-- |If the comment list is empty or the configuration option "keepAllComments" 
-- is false: Return an empty string.
-- 
-- Otherwise: Return a pretty printed sequence of comments 
-- (with a leading space, if the Bool value is True).
fComments' :: Bool -> PrettyPrinter [Comment]
fComments' _ _                      _ []         = ""
fComments' _ (Config _ _ _ False _) _ _          = ""
fComments' s conf                   c (cm : cms) = if s
  then " "
  else
    ""
    ++ fComment conf c cm
    ++ concatMap (fLineComment conf c) cms
    ++ indent conf c

-- |Calls fComments' with the Bool value False.
fComments :: PrettyPrinter [Comment]
fComments conf c cms = fComments' False conf c cms

-- |Calls fComments' with the Bool value True.
fComments_ :: PrettyPrinter [Comment]
fComments_ conf c cms = fComments' True conf c cms



-- ** AST nodes
-----------------------------------------------------

-- Program ----------------------------------------------------

-- |Returns the pretty printed source code of an SPL program by traversing 
-- its underlying AST.
fProgram :: PrettyPrinter Program
fProgram conf c (Program gl) = concatMap (fGlobalDeclaration conf c) gl


-- GlobalDeclarations -----------------------------------------

fGlobalDeclaration :: PrettyPrinter (Commented GlobalDeclaration)
fGlobalDeclaration conf c (TypeDeclaration s t, css) =
  indent conf c
    ++ "type "
    ++ fComments conf c (head css)
    ++ s
    ++ " "
    ++ fComments conf c (css !! 1)
    ++ "= "
    ++ fComments conf c (css !! 2)
    ++ fTypeExpression conf c t
    ++ ";"
    ++ fOptionalComment_ conf c (css !! 3)
fGlobalDeclaration conf c (ProcedureDeclaration i p v s, css) =
  let c1 = c + 1
  in  indent conf c
        ++ "proc "
        ++ fComments conf c (head css)
        ++ i
        ++ noSpaceIfEmpty (css !! 1)
        ++ fComments conf c (css !! 1)
        ++ "("
        ++ noSpaceIfEmpty (css !! 2)
        ++ fComments conf c (css !! 2)
        ++ fParameterDeclarations conf c1 p
        ++ ") "
        ++ fComments conf c (css !! 3)
        ++ "{"
        ++ noSpaceIfEmpty (css !! 4)
        ++ fComments conf c (css !! 4)
        ++ newLineIfEmpty (css !! 4) conf
        ++ concatMap (fVariableDeclaration conf c1) v
        ++ concatMap (fStatement_ conf c1)          s
        ++ indent conf c
        ++ "}"
        ++ noSpaceIfEmpty (css !! 5)
        ++ fOptionalComment_ conf c (css !! 5)

fGlobalDeclaration conf _ (GlobalEmptyLine, _) = newline_ conf
fGlobalDeclaration conf c (GlobalComment s, _) = fLineComment conf c s


-- TypeExpressions --------------------------------------------

fTypeExpression :: PrettyPrinter (Commented TypeExpression)
fTypeExpression conf@(Config it n _ _ _) c (NamedTypeExpression s, css) =
  s ++ noSpaceIfEmpty (head css) ++ fComments conf c (head css)
fTypeExpression conf c (ArrayTypeExpression s t, css) =
  "array"
    ++ noSpaceIfEmpty (head css)
    ++ fComments conf c (head css)
    ++ "["
    ++ noSpaceIfEmpty (css !! 1)
    ++ fComments conf c (css !! 1)
    ++ showIntLiteral s
    ++ noSpaceIfEmpty (css !! 2)
    ++ fComments conf c (css !! 2)
    ++ "] "
    ++ fComments conf c (css !! 3)
    ++ "of "
    ++ fComments conf c (css !! 4)
    ++ fTypeExpression conf c t


-- ParameterDeclaration ---------------------------------------

fParameterDeclarations :: PrettyPrinter [(Commented ParameterDeclaration)]
fParameterDeclarations _    _ []  = ""
fParameterDeclarations conf c [p] = fParameterDeclaration conf c p
fParameterDeclarations conf c pl =
  fParameterDeclaration conf c (head pl) ++ ", " ++ fParameterDeclarations
    conf
    c
    (tail pl)

fParameterDeclaration :: PrettyPrinter (Commented ParameterDeclaration)
fParameterDeclaration conf c (ParameterDeclaration s t b, css) =
  (if b then "ref " else "")
    ++ fComments conf c (head css)
    ++ s
    ++ noSpaceIfEmpty (css !! 1)
    ++ fComments conf c (css !! 1)
    ++ ": "
    ++ fComments conf c (css !! 2)
    ++ fTypeExpression conf c t


-- VariableDeclaration ----------------------------------------

fVariableDeclaration :: PrettyPrinter (Commented VariableDeclaration)
fVariableDeclaration conf c (VariableDeclaration s t, css) =
  indent conf c
    ++ "var "
    ++ fComments conf c (head css)
    ++ s
    ++ noSpaceIfEmpty (css !! 1)
    ++ fComments conf c (css !! 1)
    ++ ": "
    ++ fComments conf c (css !! 2)
    ++ fTypeExpression conf c t
    ++ ";"
    ++ fOptionalComment_ conf c (css !! 3)
fVariableDeclaration conf c (VariableDeclarationComment cs, _) =
  fLineComment conf c cs


-- Statements -------------------------------------------------

fStatement_ :: PrettyPrinter (Commented Statement)
fStatement_ conf c s@(StatementEmptyLine, _) = fStatement conf c s
fStatement_ conf c s = indent conf c ++ fStatement conf c s

fStatement :: PrettyPrinter (Commented Statement)
fStatement conf c (AssignStatement v e, css) =
  fVariable conf c v
    ++ spaceIfEmpty (peekLastComment v)
    ++ ":= "
    ++ fComments conf c (head css)
    ++ fExpression conf c e
    ++ ";"
    ++ fOptionalComment_ conf c (css !! 1)

fStatement conf c (CallStatement s es, css) =
  s
    ++ noSpaceIfEmpty (head css)
    ++ fComments conf c (head css)
    ++ "("
    ++ noSpaceIfEmpty (css !! 1)
    ++ fComments conf c (css !! 1)
    ++ fExpressionList conf c es
    ++ ")"
    ++ noSpaceIfEmpty (css !! 2)
    ++ fComments conf c (css !! 2)
    ++ ";"
    ++ fOptionalComment_ conf c (css !! 3)

fStatement conf c s@(CompoundStatement _, _) = fCurlyStatement True conf c s

fStatement conf c (EmptyStatement, css) =
  ";" ++ fOptionalComment_ conf c (head css)

fStatement conf c (IfStatement e s@(CompoundStatement _, _) s'@(Just _), css) =
  fConditionHead "if" conf c (e, take 3 css)
    ++ fCurlyStatement False conf c s
    ++ ifEmptyElse (peekLastComment s) "" (indent conf c)
    ++ fElse conf c (s', css !! 3)

fStatement conf c (IfStatement e s ms, css) =
  fConditionHead "if" conf c (e, take 3 css) ++ fStatement conf c s ++ _fElse
    conf
    c
    (ms, css !! 3)

fStatement conf c (WhileStatement e s, css) =
  fConditionHead "while" conf c (e, take 3 css) ++ fStatement conf c s

fStatement conf c (StatementComment s, _) = fComment conf c s

fStatement conf _ (StatementEmptyLine, _) = newline_ conf

_fElse :: PrettyPrinter (Maybe (Commented Statement), [Comment])
_fElse conf c (Nothing, _) = ""
_fElse conf c s            = indent conf c ++ fElse conf c s

fElse :: PrettyPrinter (Maybe (Commented Statement), [Comment])
fElse conf c (Nothing, _) = ""
fElse conf c (Just stmd, cs) =
  "else " ++ fComments conf c cs ++ fStatement conf c stmd

-- |Returns the pretty printed head of a conditional statement
--
-- For the head of an if-statement: provide "if" as the first argument. 
-- For the head of a while statement: provide "while" as the first argument.
fConditionHead :: String -> PrettyPrinter (Commented Expression, [[Comment]])
fConditionHead s conf c (e, css) =
  s
    ++ " "
    ++ fComments conf c (head css)
    ++ "("
    ++ noSpaceIfEmpty (css !! 1)
    ++ fComments conf c (css !! 1)
    ++ fExpression conf c e
    ++ ") "
    ++ fComments conf c (css !! 2)

-- |Takes a Bool argument and returns a pretty printed compound statement 
-- in one of two versions.
--
-- Usually, a statement is immediately followed by a newline (or a comment),
-- but there is one exception: An if-statement with a compound statement
-- followed by an else-statement. In this case, the closing curly brace of 
-- the compound statement is supposed to be followed by the "else"-keyword 
-- in the same line. If the first argument is True, the compound statement 
-- is printed in the usual way, otherwise it is printed in the context of an
-- if-else-block.
fCurlyStatement :: Bool -> PrettyPrinter (Commented Statement)
fCurlyStatement b conf c (CompoundStatement sts, css) =
  "{"
    ++ fOptionalComment_ conf c (head css)
    ++ concatMap (\s -> fStatement_ conf (c + 1) s) sts
    ++ indent conf c
    ++ "}"
    ++ let css1 = (css !! 1)
       in  if b
             then fOptionalComment_ conf c css1
             else fOptionalComment conf c css1
fCurlyStatement _ _ _ _ = "{}"


-- Variables --------------------------------------------------

fVariable :: PrettyPrinter (Commented Variable)
fVariable conf c (NamedVariable v, css) =
  v ++ noSpaceIfEmpty (head css) ++ fComments conf c (head css)
fVariable conf c (ArrayAccess v expr, _) =
  fVariable conf c v ++ fBracketExpression conf c expr

fBracketExpression :: PrettyPrinter (Commented Expression)
fBracketExpression conf c (expr, css) =
  "["
    ++ noSpaceIfEmpty (head css)
    ++ fComments conf c (head css)
    ++ fExpression conf c (expr, init . tail $ css)
    ++ "]"
    ++ noSpaceIfEmpty (last css)
    ++ fComments conf c (last css)


-- Expressions ------------------------------------------------

fExpression :: PrettyPrinter (Commented Expression)
fExpression conf c (VariableExpression v, _) = fVariable conf c v
fExpression conf c (IntLiteral i, css) =
  showIntLiteral i ++ noSpaceIfEmpty (head css) ++ fComments conf c (head css)
fExpression conf c (Parenthesized expr, css) =
  "("
    ++ noSpaceIfEmpty (head css)
    ++ fComments conf c (head css)
    ++ fExpression conf c expr
    ++ ")"
    ++ noSpaceIfEmpty (css !! 1)
    ++ fComments conf c (css !! 1)
fExpression conf c (BinaryExpression op expr1 expr2, _) =
  fExpression conf c expr1
    ++ spaceIfEmpty (peekLastComment expr1)
    ++ fOperator conf c op
    ++ fExpression conf c expr2
fExpression conf c (Negative expr, css) =
  showOp Minus
    ++ noSpaceIfEmpty (head css)
    ++ fComments conf c (head css)
    ++ fExpression conf c expr
fExpression conf c (Positive expr, css) =
  (if removeUnnecessarySigns conf
      then showOp Plus ++ noSpaceIfEmpty (head css)
      else ""
    )
    ++ fComments conf c (head css)
    ++ fExpression conf c expr

fExpressionList :: PrettyPrinter [Commented Expression]
fExpressionList conf c []     = ""
fExpressionList conf c [expr] = fExpression conf c expr
fExpressionList conf c (e : es) =
  fExpression conf c e ++ concatMap (fCommaExpression conf c) es

fCommaExpression :: PrettyPrinter (Commented Expression)
fCommaExpression conf c (e, css) =
  ", " ++ fComments conf c (css !! 1) ++ fExpression conf c (e, tail css)


-- Operators --------------------------------------------------

fOperator :: PrettyPrinter (Commented Op)
fOperator conf c (op, css) = showOp op ++ " " ++ fComments conf c (head css)

fOperator_ :: PrettyPrinter (Commented Op)
fOperator_ conf c opC = " " ++ fOperator conf c opC



--------------------------------------------------------------
-- * Utility functions
--------------------------------------------------------------

-- |Returns a specific number of tabs or spaces depending on the configuration.
-- parameter and the indentation level.
indent :: Config -> IndentationLevel -> String
indent conf n = replicate (n * indentNum conf) $ case (indentType conf) of
  Space -> ' '
  Tab   -> '\t'

-- |Returns a new line depending on the configuration parameter.
newline_ :: Config -> String
newline_ conf = case newlineEncoding conf of
  Linux      -> "\n"
  Windows    -> "\r\n"
  ClassicMac -> "\r"

-- |Converts an operator into a printable symbol.
showOp :: Op -> String
showOp Lt    = "<"
showOp Ne    = "#"
showOp Plus  = "+"
showOp Slash = "/"
showOp Star  = "*"
showOp Gt    = ">"
showOp Le    = "<="
showOp Minus = "-"
showOp Ge    = ">="
showOp Eq    = "="

-- |Returns an IntLiteral string.
showIntLiteral :: String -> String
showIntLiteral "'\n'" = "'\\n'"
showIntLiteral s      = s

-- |If the list is empty then return a, otherwise b.
ifEmptyElse :: [a] -> b -> b -> b
ifEmptyElse xs a b = if null xs then a else b

-- |If the passed list is empty, then return a space, otherwise an empty string.
spaceIfEmpty :: [a] -> String
spaceIfEmpty xs = ifEmptyElse xs " " ""

-- |If the passed list is empty, then return an empty string, otherwise a space.
noSpaceIfEmpty :: [a] -> String
noSpaceIfEmpty xs = ifEmptyElse xs "" " "

-- |If the passed list is empty, then return a new line.
newLineIfEmpty :: [a] -> Config -> String
newLineIfEmpty xs conf = ifEmptyElse xs (newline_ conf) ""

-- |Returns the last comment list of a Commented value.
peekLastComment :: Commented a -> [Comment]
peekLastComment (_, css) = last css
