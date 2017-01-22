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

  putStrLn $ prettyPrint $ rewriteCompilationUnit rules javaCompilationUnit

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


readJavaFile :: FilePath -> IO CompilationUnit
readJavaFile javaFile = do
  ejava <- parse (Parser.compilationUnit <* eof) javaFile <$> Lexer.lexer <$> readFile javaFile
  case ejava of
    Right java -> return java
    Left err -> do
      hPutStrLn stderr $ "Error while loading .java file\n" ++ (show err)
      exitWith (ExitFailure 1)
