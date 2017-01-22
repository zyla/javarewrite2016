{-# LANGUAGE DeriveFunctor, FlexibleContexts #-}
module JavaRewrite.Rewrite where

import Control.Monad

import Language.Java.Syntax

import JavaRewrite.Rule
import JavaRewrite.Match
import JavaRewrite.ApplySubst
import JavaRewrite.Traversals

newtype Apply a = Apply (Bool, a) deriving (Eq, Show, Functor)

instance Applicative Apply where
  pure = return
  (<*>) = ap

instance Monad Apply where
  return x = Apply (False, x)
  Apply (s1, e1) >>= k
    | Apply (s2, e2) <- k e1
       = Apply (s1 || s2, e2)

applySuccess :: a -> Apply a
applySuccess exp = Apply (True, exp)

getResult :: Apply a -> a
getResult (Apply (_, a)) = a

isSuccess :: Apply a -> Bool
isSuccess (Apply (b, _)) = b

-- | Apply a rule immediately to the expression.
-- Returns @Just new_expr@ if it succeeded, @Nothing@ otherwise.
applyRule :: Rule -> Exp -> Apply Exp
applyRule (Rule pattern rhs) exp =
    case match pattern exp of
      Just subst -> applySuccess (applySubst subst rhs)
      Nothing    -> return exp

-- | Apply a set of rules to an expression in turn. Succeeds if any of the rules succeeded.
applyRules :: [Rule] -> Exp -> Apply Exp
applyRules = foldr (>=>) return . map applyRule

-- | Repeat a transformation until it fails.
-- Succeeds if the transformation succeeded at least once.
repeatUntilFailure :: (a -> Apply a) -> a -> Apply a
repeatUntilFailure transform x =
  let result = transform x
  in if isSuccess result
     then repeatUntilFailure transform (getResult result) >>= applySuccess
     else result

-- | Apply rules to each subexpression in a top-down manner.
applyRulesTopdown :: [Rule] -> Exp -> Apply Exp
applyRulesTopdown = repeatUntilFailure . topdown subexpressions . repeatUntilFailure . applyRules

-- | Apply a set of rules to a value containing expressions.
rewrite :: Substructure Exp a => [Rule] -> a -> Apply a
rewrite rules = expressions (applyRulesTopdown rules)

rewriteCompilationUnit :: [Rule] -> CompilationUnit -> CompilationUnit
rewriteCompilationUnit rules = getResult . rewrite rules
