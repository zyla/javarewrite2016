module Main where

import Control.Monad
import qualified Data.Map as M
import Text.Parsec

import System.IO (hPutStrLn, stderr)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))

import JavaRewrite
import JavaRewrite.RuleParser

import qualified Language.Java.Parser as Parser
import qualified Language.Java.Lexer as Lexer
import Language.Java.Pretty
import Language.Java.Syntax (CompilationUnit, Ident(..))

main :: IO ()
main = do
  (rulesFile, javaFile) <- parseArgs

  rules <- readRules rulesFile
  javaCompilationUnit <- readJavaFile javaFile

  case rewriteCompilationUnit rules javaCompilationUnit of

    Left error -> failWithMessage $ showRewriteError error

    Right result -> putStrLn (prettyPrint result)

showRewriteError :: RewriteError -> String
showRewriteError (ConstantFoldingFailed detail) = "Constant folding failed: " ++ detail

parseArgs :: IO (String, String)
parseArgs = do
  args <- getArgs
  case args of
    [rulesFile, javaFile] -> return (rulesFile, javaFile)
    _ -> failWithMessage "Usage: javarewrite2016 <rules file> <java file>"


readRules :: FilePath -> IO [Rule]
readRules rulesFile = do
  erules <- parse (rules <* eof) rulesFile <$> Lexer.lexer <$> readFile rulesFile
  case erules of
    Right rules -> return rules
    Left err -> do
      failWithMessage $ "Error while loading rules' file\n" ++ show err


readJavaFile :: FilePath -> IO CompilationUnit
readJavaFile javaFile = do
  ejava <- parse (Parser.compilationUnit <* eof) javaFile <$> Lexer.lexer <$> readFile javaFile
  case ejava of
    Right java -> return java
    Left err -> do
      failWithMessage $ "Error while loading .java file\n" ++ show err

failWithMessage :: String -> IO a
failWithMessage msg = hPutStrLn stderr msg >> exitWith (ExitFailure 1)
