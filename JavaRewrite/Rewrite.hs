{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving #-}
module JavaRewrite.Rewrite where

import Control.Monad
import Data.Monoid (Any(..))

import Data.Functor.Identity
import Control.Monad.Except
import Control.Monad.Writer

import Language.Java.Syntax

import JavaRewrite.Rule
import JavaRewrite.Match
import JavaRewrite.ApplySubst
import JavaRewrite.Traversals

data RewriteError = ConstantFoldingFailed
  deriving (Show, Eq)

type Rewrite = WriterT Any (ExceptT RewriteError Identity)

markSuccess :: Rewrite ()
markSuccess = tell (Any True)

runRewrite :: Rewrite a -> Either RewriteError a
runRewrite = fmap fst . runExcept . runWriterT

-- | Rewrite a rule immediately to the expression.
-- Returns @Just new_expr@ if it succeeded, @Nothing@ otherwise.
applyRule :: Rule -> Exp -> Rewrite Exp
applyRule (Rule pattern rhs) exp =
    case match pattern exp of
      Just subst -> markSuccess >> return (applySubst subst rhs)
      Nothing    -> return exp

-- | Rewrite a set of rules to an expression in turn. Succeeds if any of the rules succeeded.
applyRules :: [Rule] -> Exp -> Rewrite Exp
applyRules = foldr (>=>) return . map applyRule

-- | Repeat a transformation until it fails.
-- Succeeds if the transformation succeeded at least once.
repeatUntilFailure :: (a -> Rewrite a) -> a -> Rewrite a
repeatUntilFailure transform x = do
  (x', Any success) <- lift $ runWriterT (transform x)
  if success
    then do
      markSuccess
      repeatUntilFailure transform x'
    else return x'

-- | Rewrite rules to each subexpression in a top-down manner.
applyRulesTopdown :: [Rule] -> Exp -> Rewrite Exp
applyRulesTopdown = repeatUntilFailure . topdown subexpressions . repeatUntilFailure . applyRules

-- | Rewrite a set of rules to a value containing expressions.
rewrite :: Substructure Exp a => [Rule] -> a -> Either RewriteError a
rewrite rules = runRewrite . expressions (applyRulesTopdown rules)

rewriteCompilationUnit :: [Rule] -> CompilationUnit -> Either RewriteError CompilationUnit
rewriteCompilationUnit = rewrite
