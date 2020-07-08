module Main where

import           System.Environment
import           Formatter
import           Parser
import           Text.Parsec.Token
import           Text.Parsec
import           Text.Parsec.Language
import qualified Options.Applicative as O
import           Data.Semigroup ((<>))

main :: IO ()
main = do
  (conf, f, inp) <- O.execParser $ O.info (cliOpts O.<**> O.helper) mempty
  inp' <- if f 
            then readFile inp
            else return $ removeEscaped inp
  applyFormatter conf inp'


-------------------------------------------------------------------------------
-- CLI arguments
-------------------------------------------------------------------------------

-- |Uses functions from the optparse-applicative library <https://hackage.haskell.org/package/optparse-applicative> 
-- for parsing command line options and automatically generating a usage / help text
cliOpts :: O.Parser (Config, Bool, String) 
cliOpts = (, ,) 
          <$> config
          <*> O.switch
            ( O.short 'f'
            <> O.help "read Input from file" )
          <*> O.argument O.str (O.metavar "INPUT")

-- |Command line options parser for the configuration options.
config :: O.Parser Config 
config = Config
         <$> O.option parseIndentType
            ( O.long "itype" 
            <> O.value Space 
            <> O.help "Indentation type ('s' for space, 't' for tab)" )
         <*> O.option O.auto
            ( O.long "inum"
            <> O.value 2
            <> O.help "Number of spaces/tabs per indentation" 
            <> O.metavar "INT" )
         <*> O.switch
            ( O.long "rms"
            <> O.help "Remove all unnecessary signs from expressions" )
         <*> O.switch
            ( O.long "kc"
            <> O.help "Retain all misplaced comments from the source code" )
         <*> O.option parseNewLineEncoding
            ( O.long "nls"
            <> O.value Linux
            <> O.help "Newline encoding (\"linux\" => '\\n', \"win\" => '\\r\\n' or \"mac\" => '\\r')" )

-- |Parser for the indentation type option.
parseIndentType :: O.ReadM IndentationType
parseIndentType = O.eitherReader $ 
  \s -> case s of 
          "s"       -> Right Space
          "t"       -> Right Tab
          otherwise -> Left $ "Invalid indentation type: expected 's' or 't'"

-- |Parser for the newline encoding option.
parseNewLineEncoding :: O.ReadM NewlineEncoding
parseNewLineEncoding = O.eitherReader $ 
  \s -> case s of 
          "linux"   -> Right Linux
          "win"     -> Right Windows
          "mac"     -> Right ClassicMac    
          otherwise -> Left $ "Invalid newline encoding: expected 'linux', 'win' or 'mac'"


--------------------------------------------------------------
-- * Utility functions
--------------------------------------------------------------

removeEscaped :: String -> String
removeEscaped s = let p = stringLiteral $ makeTokenParser haskellDef in 
  case runParser p () "" ("\"" ++ s ++ "\"") of
    Left  err -> error ("Invalid input: \n" ++ show err)
    Right r   -> r
  

applyFormatter :: Config -> String -> IO()
applyFormatter conf inp =
   putStr $ case runParser pProgram () "" inp of
      Left  err -> error ("Parser failed: \n" ++ show err)
      Right r   -> fProgram conf 0 r