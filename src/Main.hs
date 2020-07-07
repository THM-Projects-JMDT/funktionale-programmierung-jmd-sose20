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
  args <- getArgs
  putStr $ case run2 pProgram (removeEscaped $ head args) of
    Left  err -> error (show err)
    Right r   -> fProgram defaultConfig 0 r

removeEscaped :: String -> String
removeEscaped s = case runParser p () "" ("\"" ++ s ++ "\"") of
  Left  err -> "Parser failed: " ++ show err
  Right r   -> r

p = stringLiteral $ makeTokenParser haskellDef

f :: O.Parser String
f = undefined