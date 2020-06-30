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

                            
-- for testing -----------------------------------------------
--------------------------------------------------------------

run :: Parser a -> String -> Either ParseError a
run p = runParser p () ""

testFormat :: String -> Parser a -> PrettyPrinter a -> IO ()
testFormat s p f = putStrLn $ case run p s of 
                                Left _  -> error "Parser failed"
                                Right r -> f defaultConfig 0 r



-- pretty printing -------------------------------------------
--------------------------------------------------------------


-- Comments --------------------------------------------------

fLineComment :: PrettyPrinter Comment
fLineComment conf@(Config it n _ _ _) c cm = indent it n c 
                                              ++ fComment conf c cm

fComment :: PrettyPrinter Comment
fComment (Config _ _ _ _ nls) _ c = "//" ++ c ++ case nls of
                                                       Linux      -> "\n"
                                                       Windows    -> "\r\n"
                                                       ClassicMac -> "\r"

fComments :: PrettyPrinter [Comment]
fComments _ _ []                     = ""
fComments (Config _ _ _ False _) _ _ = ""
fComments conf c (cm:cms)            = " " 
                                        ++ fComment conf c cm 
                                        ++ concatMap (fComment conf c) cms

-- Programm ---------------------------------------------------

-- GlobalDeclarations -----------------------------------------

-- TypeExpressions --------------------------------------------

-- ParameterDeclaration ---------------------------------------

-- VariableDeclaration ----------------------------------------

-- Statements -------------------------------------------------
fStatement :: PrettyPrinter (Commented Statement)  
fStatement conf@(Config it n _ _ _) c (AssignStatement v e , css) =  fVariable conf c v 
                                                                ++ " := "
                                                                ++ fExpression conf c e
                                                                ++ fComments conf c (head css)
                                                                --- todo  "x:= //hallo \n y \n ;\n kommentar fehlt

-- Variables --------------------------------------------------

fVariable :: PrettyPrinter (Commented Variable) 
fVariable conf@(Config it n _ _ _) c (NamedVariable v, css) =  v 
                                                                ++ fComments conf c (head css)

-- Expressions ------------------------------------------------

fExpression :: PrettyPrinter (Commented Expression) 
fExpression conf@(Config it n _ _ _) c (VariableExpression v, css) = fVariable conf c v
                                                        
