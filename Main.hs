module Main where

import Control.Monad
import JavaRewrite
import JavaRewrite.RuleParser
import JavaRewrite.Rule
import qualified Language.Java.Parser as Parser
import qualified Language.Java.Lexer as Lexer
import Language.Java.Pretty
import Text.Parsec
import System.Environment

main :: IO ()
main = do
  (rulesFile : javaFile : []) <- getArgs
  rules <- fromEither <$> parse (rules <* eof) rulesFile <$> Lexer.lexer <$> readFile rulesFile
  mapM_ (putStrLn . prettyPrint) rules

  javaExpr <- fromEither <$> parse (Parser.exp <* eof) javaFile <$> Lexer.lexer <$> readFile javaFile
  putStrLn $ prettyPrint javaExpr

  forM_ rules $ \rule -> do
    print $ rule_pattern rule
    print $ rule_replacement rule
    print $ match rule javaExpr

fromEither (Left err) = error (show err)
fromEither (Right val) = val
