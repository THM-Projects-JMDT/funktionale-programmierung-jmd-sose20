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
  (conf, inp) <- O.execParser $ O.info (cliArgs O.<**> O.helper) O.fullDesc
  putStr $ case run2 pProgram (removeEscaped $ inp) of
    Left  err -> error (show err)
    Right r   -> fProgram conf 0 r

removeEscaped :: String -> String
removeEscaped s = case runParser p () "" ("\"" ++ s ++ "\"") of
  Left  err -> "Parser failed: " ++ show err
  Right r   -> r

p = stringLiteral $ makeTokenParser haskellDef


-------------------------------------------------------------------------------
-- CLI arguments
-------------------------------------------------------------------------------

parseIndentType :: O.ReadM IndentationType
parseIndentType = O.eitherReader $ 
  \s -> case s of 
          "s"       -> Right Space
          "t"       -> Right Tab
          otherwise -> Left $ "Invalid Indentation type: expected 's' or 't'"

parseNewLineEncoding :: O.ReadM NewlineEncoding
parseNewLineEncoding = O.eitherReader $ 
  \s -> case s of 
          "linux"   -> Right Linux
          "win"     -> Right Windows
          "mac"     -> Right ClassicMac    
          otherwise -> Left $ "Invalid newline style: expected 'linux', 'win' or 'mac'"

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
  
cliArgs :: O.Parser (Config, String) 
cliArgs = (,) 
          <$> config
          <*> O.argument O.str (O.metavar "INPUT")
  