{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving #-}
module JavaRewrite.Rewrite where

import Control.Monad
import Data.Monoid (Any(..))

import Language.Java.Syntax

import JavaRewrite.Rule
import JavaRewrite.Match
import JavaRewrite.ApplySubst
import JavaRewrite.Traversals

newtype RewriteResult a = RewriteResult (Any, a) deriving (Eq, Show, Functor, Applicative, Monad)

rewriteSuccess :: a -> RewriteResult a
rewriteSuccess exp = RewriteResult (Any True, exp)

getResult :: RewriteResult a -> a
getResult (RewriteResult (_, a)) = a

isSuccess :: RewriteResult a -> Bool
isSuccess (RewriteResult (Any b, _)) = b

-- | RewriteResult a rule immediately to the expression.
-- Returns @Just new_expr@ if it succeeded, @Nothing@ otherwise.
applyRule :: Rule -> Exp -> RewriteResult Exp
applyRule (Rule pattern rhs) exp =
    case match pattern exp of
      Just subst -> rewriteSuccess (applySubst subst rhs)
      Nothing    -> return exp

-- | RewriteResult a set of rules to an expression in turn. Succeeds if any of the rules succeeded.
applyRules :: [Rule] -> Exp -> RewriteResult Exp
applyRules = foldr (>=>) return . map applyRule

-- | Repeat a transformation until it fails.
-- Succeeds if the transformation succeeded at least once.
repeatUntilFailure :: (a -> RewriteResult a) -> a -> RewriteResult a
repeatUntilFailure transform x =
  let result = transform x
  in if isSuccess result
     then repeatUntilFailure transform (getResult result) >>= rewriteSuccess
     else result

-- | RewriteResult rules to each subexpression in a top-down manner.
applyRulesTopdown :: [Rule] -> Exp -> RewriteResult Exp
applyRulesTopdown = repeatUntilFailure . topdown subexpressions . repeatUntilFailure . applyRules

-- | RewriteResult a set of rules to a value containing expressions.
rewrite :: Substructure Exp a => [Rule] -> a -> RewriteResult a
rewrite rules = expressions (applyRulesTopdown rules)

rewriteCompilationUnit :: [Rule] -> CompilationUnit -> CompilationUnit
rewriteCompilationUnit rules = getResult . rewrite rules
