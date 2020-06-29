module SPLFormatter where

import SPLAbsyn

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
    keepAllComments :: Bool
} deriving (Eq, Show)

defaultConfig = Config Tab 1 False False

data IndentationType = Space 
                     | Tab 
                     deriving (Eq, Show)

type PrettyPrinter a = Config -> Int -> a -> String



-- utility ---------------------------------------------------
--------------------------------------------------------------

indent :: IndentationType -> Int -> Int -> String 
indent it n c = replicate (n * c) $ case it of 
                                      Space -> ' '
                                      Tab   -> '\t'

showComment :: Comment -> String
showComment c = "//" ++ c ++ "\n"



-- pretty printing -------------------------------------------
--------------------------------------------------------------

printVariable :: PrettyPrinter (Commented Variable) 
printVariable conf@(Config it n _ _) c (NamedVariable v, css) = indent it n c
                                                             ++ v 
                                                             ++ printComments conf c (head css)

printLineComment :: PrettyPrinter Comment
printLineComment (Config it n _ _) c cm = indent it n c 
                                       ++ showComment cm)

printComments :: PrettyPrinter [Comment]
printComments _ _ [] = ""
printComments conf c (cm:cms) = " " 
                             ++ showComment cm 
                             ++ concatMap (printLineComment conf c) cms