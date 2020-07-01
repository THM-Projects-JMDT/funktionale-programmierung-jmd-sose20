module SPLFormatter where

import SPLAbsyn
import Text.Parsec
import SPLParser

{-
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Pretty Printer for the SPL-AST
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-}


-- configuration for the pretty printer
data Config = Config {
    indentType :: IndentationType,
    indentNum :: Int,
    leftCurlNextLine :: Bool,
    keepAllComments :: Bool,
    newLineType :: NewLineStyle
} deriving (Eq, Show)

defaultConfig = Config Space 2 False True Linux

data IndentationType = Space 
                     | Tab 
                     deriving (Eq, Show)

data NewLineStyle = Linux
                  | Windows
                  | ClassicMac
                  deriving (Eq, Show)

type PrettyPrinter a = Config -> IndentationLevel -> a -> String

type IndentationLevel = Int


-- utility ---------------------------------------------------
--------------------------------------------------------------

indent :: IndentationType -> IndentationLevel -> Int -> String 
indent it n c = replicate (n * c) $ case it of 
                                      Space -> ' '
                                      Tab   -> '\t'

newline_ :: NewLineStyle -> String 
newline_ nls = case nls of
                 Linux      -> "\n"
                 Windows    -> "\r\n"
                 ClassicMac -> "\r"

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
                            
-- for testing -----------------------------------------------
--------------------------------------------------------------

run :: Parser a -> String -> Either ParseError a
run p = runParser p () ""

testFormat :: String -> Parser a -> PrettyPrinter a -> IO ()
testFormat s p f = putStrLn $ case run p s of 
                                Left err  -> "Parser failed: " ++ show err
                                Right r -> f defaultConfig 2 r



-- pretty printing -------------------------------------------
--------------------------------------------------------------


-- Comments --------------------------------------------------

fLineComment :: PrettyPrinter Comment
fLineComment conf@(Config it n _ _ _) c cm = indent it n c ++ fComment conf c cm

fComment :: PrettyPrinter Comment
fComment (Config _ _ _ _ nls) _ c = "//" ++ c ++ newline_ nls

fComment_ :: PrettyPrinter Comment 
fComment_ conf c cm = " " ++ fComment conf c cm

fOptionalComment :: PrettyPrinter [Comment]
fOptionalComment conf@(Config _ _ _ _ nls) c cs = if null cs 
                                                    then newline_ nls 
                                                    else fComment_ conf c (head cs)

fComments :: PrettyPrinter [Comment]
fComments _ _ []                              = ""
fComments (Config _ _ _ False _) _ _          = ""
fComments conf@(Config it n _ _ _) c (cm:cms) = " " 
                                             ++ fComment conf c cm 
                                             ++ concatMap (fLineComment conf c) cms
                                             ++ indent it n c

-- Programm ---------------------------------------------------

-- GlobalDeclarations -----------------------------------------

-- TypeExpressions --------------------------------------------

-- ParameterDeclaration ---------------------------------------

-- VariableDeclaration ----------------------------------------

-- Statements -------------------------------------------------
fStatement :: PrettyPrinter (Commented Statement)  
fStatement conf@(Config it n _ _ nls) c (AssignStatement v e, css) = indent it n c
                                                                  ++ fVariable conf c v 
                                                                  ++ " := "
                                                                  ++ fComments conf c (head css)
                                                                  ++ fExpression conf c e
                                                                  ++ ";"
                                                                  ++ fOptionalComment conf c (css !! 1)

fStatement conf c (StatementComment s, _)                         = fLineComment conf c s    

fStatement conf@(Config _ _ _ _ nls) _ (StatementEmptyLine, _)    = newline_ nls

fStatement conf@(Config it n _ _ nls) c (EmptyStatement, css)     = indent it n c
                                                                 ++ ";"
                                                                 ++ fOptionalComment conf c (head css)                                                    
                                                              

-- Variables --------------------------------------------------

fVariable :: PrettyPrinter (Commented Variable) 
fVariable conf c (NamedVariable v, css)  = v 
                                        ++ fComments conf c (head css)
fVariable conf c (ArrayAccess v expr, _) = fVariable conf c v
                                        ++ fBracketExpression conf c expr
                                                
fBracketExpression :: PrettyPrinter (Commented Expression)
fBracketExpression conf c (expr, css) = "[" 
                                     ++ fComments conf c (head css) 
                                     ++ fExpression conf c (expr, init . tail $ css) 
                                     ++ "]"
                                     ++ fComments conf c (last css )

-- Expressions ------------------------------------------------

fExpression :: PrettyPrinter (Commented Expression) 
fExpression conf c (VariableExpression v, _) = fVariable conf c v
fExpression conf c (IntLiteral i, css) =                  i
                                                       ++ fComments conf c (head css)
fExpression conf c (Parenthesized expr, css) =            "("
                                                       ++ fComments conf c (head css)
                                                       ++ fExpression conf c expr
                                                       ++ ")"
                                                       ++ fComments conf c (css !! 1)       
fExpression conf c (BinaryExpression op expr1 expr2, _) = fExpression conf c expr1
                                                       ++ fOperator conf c op
                                                       ++ fExpression conf c expr2
fExpression conf c (Negative expr, css) =                 showOp Minus
                                                       ++ fComments conf c (head css)
                                                       ++ fExpression conf c expr
fExpression conf c (Positive expr, css) =                 showOp Plus
                                                       ++ fComments conf c (head css)
                                                       ++ fExpression conf c expr                             

-- Operator ---------------------------------------------------

fOperator :: PrettyPrinter (Commented Op)
fOperator conf c (op, css) = " "
                           ++ showOp op
                           ++ " "
                           ++ fComments conf c (head css) 