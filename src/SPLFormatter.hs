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

printVariable :: PrettyPrinter (Commented Variable) 
printVariable conf@(Config it n _ _ _) c (NamedVariable v, css) =  v 
                                                                ++ printComments conf c (head css)

printLineComment :: PrettyPrinter Comment
printLineComment conf@(Config it n _ _ _) c cm = indent it n c 
                                              ++ printComment conf c cm

printComment :: PrettyPrinter Comment
printComment (Config _ _ _ _ nlt) _ c = "//" ++ c ++ case nlt of
                                                       Linux      -> "\n"
                                                       Windows    -> "\r\n"
                                                       ClassicMac -> "\r"

printComments :: PrettyPrinter [Comment]
printComments _ _ []                     = ""
printComments (Config _ _ _ False _) _ _ = ""
printComments conf c (cm:cms)            = " " 
                                        ++ printComment conf c cm 
                                        ++ concatMap (printLineComment conf c) cms
