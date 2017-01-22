{-
   Module for parsing and applying rules in actual code
-}
module JavaRewrite.Rewriter where

import JavaRewrite.Rule
import JavaRewrite.MatchResult

import Language.Java.Syntax
import Language.Java.Parser
import qualified Language.Java.Parser as Parser
import qualified Language.Java.Lexer as Lexer

import Control.Monad ((>=>))

import System.IO (hPutStrLn, stderr)
import System.Exit (exitWith, ExitCode(..))


readJava :: FilePath -> IO CompilationUnit
readJava javaFile = do
  ejava <- Parser.parser compilationUnit <$> readFile javaFile
  case ejava of
    Right java -> return java
    Left err -> do
      hPutStrLn stderr $ "Error while loading .java file\n" ++ (show err)
      exitWith (ExitFailure 1)


-- | Traverse source code and apply rules
travJava :: Monad f => (Exp -> f Exp) -> CompilationUnit -> f CompilationUnit
travJava inj (CompilationUnit pdecl idecls typedecls)
  = CompilationUnit pdecl idecls <$>
    (sequence $ map (travTypeDecl inj) typedecls)


travTypeDecl :: Monad f => (Exp -> f Exp) -> TypeDecl -> f TypeDecl
travTypeDecl inj (ClassTypeDecl cdecl) =
  ClassTypeDecl <$> travClassTDecl inj cdecl
travTypeDecl inj (InterfaceTypeDecl idecl) = undefined -- TODO


travClassTDecl :: Monad f => (Exp -> f Exp) -> ClassDecl -> f ClassDecl
travClassTDecl inj (ClassDecl m i t mr r cbody) =
  ClassDecl m i t mr r <$> travClassBody inj cbody
travClassTDecl inj (EnumDecl m i r ebody) = undefined -- TODO


travClassBody :: Monad f => (Exp -> f Exp) -> ClassBody -> f ClassBody
travClassBody inj (ClassBody decls) = ClassBody <$> sequence (map (travDecl inj) decls)


travDecl :: Monad f => (Exp -> f Exp) -> Decl -> f Decl
travDecl inj (MemberDecl mdecl) = undefined -- TODO
travDecl inj (InitDecl b block) = undefined -- TODO
