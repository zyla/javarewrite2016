module Main where

import Control.Monad
import JavaRewrite
import JavaRewrite.RuleParser
import JavaRewrite.Rule
import qualified Language.Java.Parser as Parser
import qualified Language.Java.Lexer as Lexer
import Language.Java.Pretty
import Language.Java.Syntax (Exp)
import Language.Java.Syntax.Types (Ident(..))
import Text.Parsec
import System.IO (hPutStrLn, stderr)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import qualified Data.Map as M

main :: IO ()
main = do
  (rulesFile, javaFile) <- parseArgs

  rules <- readRules rulesFile
  javaExpr <- readJavaFile javaFile

  putStrLn $ "EXPRESSION:"
  putStrLn $ prettyPrint javaExpr

  forM_ rules $ \(Rule pattern _) -> do
    putStrLn $ "\nPATTERN: " ++ prettyPrint pattern
    case match pattern javaExpr of

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


readRules :: FilePath -> IO [Rule]
readRules rulesFile = do
  erules <- parse (rules <* eof) rulesFile <$> Lexer.lexer <$> readFile rulesFile
  case erules of
    Right rules -> return rules
    Left err -> do
      hPutStrLn stderr $ "Error while loading rules' file\n" ++ (show err)
      exitWith (ExitFailure 1)


readJavaFile :: FilePath -> IO Exp
readJavaFile javaFile = do
  ejava <- parse (Parser.exp <* eof) javaFile <$> Lexer.lexer <$> readFile javaFile
  case ejava of
    Right java -> return java
    Left err -> do
      hPutStrLn stderr $ "Error while loading .java file\n" ++ (show err)
      exitWith (ExitFailure 1)



-- TODO fix this ugly stuff
fromRight (Left err) = error (show err)
fromRight (Right val) = val

-- for GHCi
unsafeParse :: P a -> String -> a
unsafeParse p = fromRight . parse (p <* eof) "<input>" . Lexer.lexer
