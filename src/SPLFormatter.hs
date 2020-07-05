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

ifEmptyElse :: [a] -> b -> b -> b 
ifEmptyElse xs a b = if null xs then a else b

spaceIfEmpty :: [a] -> String
spaceIfEmpty xs = ifEmptyElse xs " " ""

noSpaceIfEmpty :: [a] -> String
noSpaceIfEmpty xs = ifEmptyElse xs "" " "

newLineIfEmpty :: [a] -> NewLineStyle -> String
newLineIfEmpty xs nls = ifEmptyElse xs (newline_ nls) ""

peekLastComment :: Commented a -> [Comment]
peekLastComment (_, css) = last css
                            
-- for testing -----------------------------------------------
--------------------------------------------------------------

run2 :: Parser a -> String -> Either ParseError a
run2 p = runParser p () ""

testFormat :: String -> Parser a -> PrettyPrinter a -> IO ()
testFormat s p f = putStrLn $ case run2 p s of 
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
                                                    then " "
                                                    else fComment_ conf c (head cs)

fOptionalComment_ :: PrettyPrinter [Comment]
fOptionalComment_ conf@(Config _ _ _ _ nls) c cs = if null cs 
                                                    then newline_ nls 
                                                    else fComment_ conf c (head cs)

fComments' :: Bool -> PrettyPrinter [Comment]
fComments' _ _ _ []                              = ""
fComments' _ (Config _ _ _ False _) _ _          = ""
fComments' s conf@(Config it n _ _ _) c (cm:cms) = if s then " " else ""
                                                   ++ fComment conf c cm 
                                                   ++ concatMap (fLineComment conf c) cms
                                                   ++ indent it n c

fComments :: PrettyPrinter [Comment]
fComments conf c cms = fComments' False conf c cms

fComments_ :: PrettyPrinter [Comment]
fComments_ conf c cms = fComments' True conf c cms

-- Programm ---------------------------------------------------

fProgram :: PrettyPrinter Program
fProgram conf@(Config it n _ _ _) c (Program gl) = concatMap (fGlobalDeclaration conf c) gl

-- GlobalDeclarations -----------------------------------------

fGlobalDeclaration :: PrettyPrinter (Commented GlobalDeclaration)
fGlobalDeclaration conf@(Config it n _ _ _) c (TypeDeclaration s t, css)          = indent it n c
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
fGlobalDeclaration conf@(Config it n _ _ nls) c (ProcedureDeclaration i p v s, css) = let c1 = c + 1 in
                                                                                      indent it n c
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
                                                                                   ++ newLineIfEmpty (css !! 4) nls
                                                                                   ++ concatMap (fVariableDeclaration conf c1) v
                                                                                   ++ concatMap (fStatement_ conf c1) s
                                                                                   ++ indent it n c
                                                                                   ++ "}"
                                                                                   ++ noSpaceIfEmpty (css !! 5)
                                                                                   ++ fOptionalComment_ conf c (css !! 5)
fGlobalDeclaration conf@(Config _ _ _ _ nls) _ (GlobalEmptyLine, _)               = newline_ nls
fGlobalDeclaration conf c (GlobalComment s, _)                                    = fLineComment conf c s   

-- TypeExpressions --------------------------------------------

fTypeExpression :: PrettyPrinter (Commented TypeExpression)
fTypeExpression conf@(Config it n _ _ _) c (NamedTypeExpression s, css) = s
                                                                          ++ noSpaceIfEmpty (head css)
                                                                          ++ fComments conf c (head css)
fTypeExpression conf@(Config it n _ _ _) c (ArrayTypeExpression s t, css) = "array" 
                                                                            ++ noSpaceIfEmpty (head css)
                                                                            ++ fComments conf c (head css)
                                                                            ++ "["
                                                                            ++ noSpaceIfEmpty (css !! 1)
                                                                            ++ fComments conf c (css !! 1)
                                                                            ++ s 
                                                                            ++ noSpaceIfEmpty (css !! 2)
                                                                            ++ fComments conf c (css !! 2)
                                                                            ++ "] "
                                                                            ++ fComments conf c (css !! 3)
                                                                            ++ "of "
                                                                            ++ fComments conf c (css !! 4)
                                                                            ++ fTypeExpression conf c t                                            
                                                            
-- ParameterDeclaration ---------------------------------------
fParameterDeclarations :: PrettyPrinter [(Commented ParameterDeclaration)]
fParameterDeclarations _ _ []                         = ""
fParameterDeclarations conf@(Config it n _ _ _) c [p] = fParameterDeclaration conf c p
fParameterDeclarations conf@(Config it n _ _ _) c pl  = fParameterDeclaration conf c (head pl)
                                                     ++ ", "
                                                     ++ fParameterDeclarations conf c (tail pl)

fParameterDeclaration :: PrettyPrinter (Commented ParameterDeclaration)
fParameterDeclaration conf@(Config it n _ _ _) c (ParameterDeclaration s t b, css) = (if b then "ref " else "")
                                                                                  ++ fComments conf c (head css)
                                                                                  ++ s 
                                                                                  ++ noSpaceIfEmpty (css !! 1)
                                                                                  ++ fComments conf c (css !! 1)
                                                                                  ++ ": "
                                                                                  ++ fComments conf c (css !! 2)
                                                                                  ++ fTypeExpression conf c t

-- VariableDeclaration ----------------------------------------

fVariableDeclaration :: PrettyPrinter (Commented VariableDeclaration)
fVariableDeclaration conf@(Config it n _ _ _) c (VariableDeclaration s t, css) = indent it n c
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
fVariableDeclaration conf c (VariableDeclarationComment cs, _) = fLineComment conf c cs                                                                              
                                                                             
-- Statements -------------------------------------------------
fStatement_ :: PrettyPrinter (Commented Statement)
fStatement_ conf c s@(StatementEmptyLine, _)                      = fStatement conf c s
fStatement_ conf@(Config it n _ _ _) c s                          = indent it n c 
                                                                    ++ fStatement conf c s 

fStatement :: PrettyPrinter (Commented Statement)  
fStatement conf@(Config it n _ _ _) c (AssignStatement v e, css)    = fVariable conf c v
                                                                    ++ spaceIfEmpty (peekLastComment v)
                                                                    ++ ":= "
                                                                    ++ fComments conf c (head css)
                                                                    ++ fExpression conf c e
                                                                    ++ ";"
                                                                    ++ fOptionalComment_ conf c (css !! 1)

fStatement conf@(Config it n _ _ nls) c (CallStatement s es, css)   = s 
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
                                                                    ++ fOptionalComment_ conf  c (css !! 3)

fStatement conf c s@(CompoundStatement _, _)                         = fCurlyStatement True conf c s 

fStatement conf@(Config it n _ _ _) c (EmptyStatement, css)          = ";"
                                                                    ++ fOptionalComment_ conf c (head css)

fStatement conf@(Config it n _ _ nls) c (IfStatement e s@(CompoundStatement _, _) s'@(Just _), css) 
                                                                     = fConditionHead "while" conf c (e, take 3 css)
                                                                    ++ fCurlyStatement False conf c s
                                                                    ++ ifEmptyElse (peekLastComment s) "" (indent it n c)
                                                                    ++ fElse conf c (s', css !! 3)                                                                                                                 

fStatement conf@(Config it n _ _ nls) c (IfStatement e s ms, css)    = fConditionHead "if" conf c (e, take 3 css) 
                                                                    ++ fStatement conf c s
                                                                    ++ _fElse conf c (ms, css !! 3)   

fStatement conf@(Config it n _ _ nls) c (WhileStatement e s, css)    = fConditionHead "while" conf c (e, take 3 css)
                                                                    ++ fStatement conf c s                                                            

fStatement conf c (StatementComment s, _)                            = fComment conf c s    

fStatement conf@(Config _ _ _ _ nls) _ (StatementEmptyLine, _)       = newline_ nls

_fElse :: PrettyPrinter (Maybe (Commented Statement), [Comment])
_fElse conf c (Nothing, _) = ""
_fElse conf@(Config it n _ _ _) c s               = indent it n c 
                                                  ++ fElse conf c s

fElse :: PrettyPrinter (Maybe (Commented Statement), [Comment])
fElse conf c (Nothing, _) = ""
fElse conf@(Config it n _ _ _)  c (Just stmd, cs) = "else "
                                                  ++ fComments conf c cs
                                                  ++ fStatement conf c stmd

fConditionHead :: String -> PrettyPrinter (Commented Expression, [[Comment]])
fConditionHead s conf@(Config it n _ _ _) c (e, css) = s
                                                       ++ " "
                                                       ++ fComments conf c (head css)
                                                       ++ "("
                                                       ++ noSpaceIfEmpty (css !! 1)
                                                       ++ fComments conf c (css !! 1)
                                                       ++ fExpression conf c e
                                                       ++ ") "
                                                       ++ fComments conf c (css !! 2)  

fCurlyStatement :: Bool -> PrettyPrinter (Commented Statement)
fCurlyStatement b conf@(Config it n _ _ _) c (CompoundStatement sts, css) = 
                                                        "{"
                                                        ++ fOptionalComment_ conf c (head css)
                                                        ++ concatMap (\s -> fStatement_ conf (c + 1) s) sts
                                                        ++ indent it n c
                                                        ++ "}"
                                                        ++ let css1 = (css !! 1) in if b 
                                                            then fOptionalComment_ conf c css1 
                                                            else fOptionalComment conf c css1
fCurlyStatement _ _ _ _ = "{}"


-- Variables --------------------------------------------------

fVariable :: PrettyPrinter (Commented Variable) 
fVariable conf c (NamedVariable v, css)  = v 
                                           ++ noSpaceIfEmpty (head css)
                                           ++ fComments conf c (head css)
fVariable conf c (ArrayAccess v expr, _) = fVariable conf c v
                                           ++ fBracketExpression conf c expr
                                                
fBracketExpression :: PrettyPrinter (Commented Expression)
fBracketExpression conf c (expr, css) = "[" 
                                        ++ noSpaceIfEmpty (head css)
                                        ++ fComments conf c (head css) 
                                        ++ fExpression conf c (expr, init . tail $ css) 
                                        ++ "]"
                                        ++ noSpaceIfEmpty (last css)
                                        ++ fComments conf c (last css )

-- Expressions ------------------------------------------------

fExpression :: PrettyPrinter (Commented Expression) 
fExpression conf c (VariableExpression v, _)            = fVariable conf c v
fExpression conf c (IntLiteral i, css)                  = i
                                                          ++ noSpaceIfEmpty (head css)
                                                          ++ fComments conf c (head css)
fExpression conf c (Parenthesized expr, css)            =  "("
                                                          ++ noSpaceIfEmpty (head css)
                                                          ++ fComments conf c (head css)
                                                          ++ fExpression conf c expr
                                                          ++ ")"
                                                          ++ noSpaceIfEmpty (css !! 1)
                                                          ++ fComments conf c (css !! 1)       
fExpression conf c (BinaryExpression op expr1 expr2, _) = fExpression conf c expr1
                                                          ++ spaceIfEmpty (peekLastComment expr1)
                                                          ++ fOperator conf c op
                                                          ++ fExpression conf c expr2
fExpression conf c (Negative expr, css)                 = showOp Minus
                                                          ++ noSpaceIfEmpty (head css)
                                                          ++ fComments conf c (head css)
                                                          ++ fExpression conf c expr
fExpression conf c (Positive expr, css)                 = showOp Plus
                                                          ++ noSpaceIfEmpty (head css)
                                                          ++ fComments conf c (head css)
                                                          ++ fExpression conf c expr     

fExpressionList :: PrettyPrinter [Commented Expression]
fExpressionList conf c []                               = ""
fExpressionList conf c [expr]                           = fExpression conf c expr    
fExpressionList conf c (e:es)                           = fExpression conf c e 
                                                          ++ concatMap (fCommaExpression conf c) es 

fCommaExpression :: PrettyPrinter (Commented Expression) 
fCommaExpression conf c (e, css)                        = ", "
                                                          ++ fComments conf c (css !! 1)
                                                          ++ fExpression conf c (e, tail css)



-- Operator ---------------------------------------------------

fOperator :: PrettyPrinter (Commented Op)
fOperator conf c (op, css) = showOp op
                          ++ " "
                          ++ fComments conf c (head css) 

fOperator_ :: PrettyPrinter (Commented Op)
fOperator_ conf c opC      = " "
                          ++ fOperator conf c opC