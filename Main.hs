module Main where

import Control.Monad
import JavaRewrite
import JavaRewrite.RuleParser
import JavaRewrite.Rule
import qualified Language.Java.Parser as Parser
import qualified Language.Java.Lexer as Lexer
import Language.Java.Pretty
import Language.Java.Syntax.Types (Ident(..))
import Text.Parsec
import System.IO (hPutStrLn, stderr)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import qualified Data.Map as M

main :: IO ()
main = do
  (rulesFile, javaFile) <- parseArgs

  -- TODO fix this ugly stuff
  rules <- fromRight <$> parse (rules <* eof) rulesFile <$> Lexer.lexer <$> readFile rulesFile

  javaExpr <- fromRight <$> parse (Parser.exp <* eof) javaFile <$> Lexer.lexer <$> readFile javaFile
  putStrLn $ "EXPRESSION:"
  putStrLn $ prettyPrint javaExpr

  forM_ rules $ \rule -> do
    putStrLn $ "\nRULE: " ++ prettyPrint rule
    case match rule javaExpr of

      Nothing ->
        putStrLn "No match"

      Just subst ->
        forM_ (M.toList subst) $ \(Ident ident, expr) ->
          putStrLn $ ident ++ " -> " ++ prettyPrint expr

parseArgs :: IO (String, String)
parseArgs = do
  args <- getArgs
  case args of
    [rulesFile, javaFile] -> return (rulesFile, javaFile)
    _ -> do
      hPutStrLn stderr "Usage: javarewrite2016 <rules file> <java file>"
      exitWith (ExitFailure 1)

-- TODO fix this ugly stuff
fromRight (Left err) = error (show err)
fromRight (Right val) = val

-- for GHCi
unsafeParse :: P a -> String -> a
unsafeParse p = fromRight . parse (p <* eof) "<input>" . Lexer.lexer
