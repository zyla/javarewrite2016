{-# OPTIONS_GHC -Werror -Wincomplete-patterns #-}
{-# LANGUAGE FlexibleContexts #-}
module JavaRewrite.ApplySubst where

import qualified Data.Map as M
import Language.Java.Syntax
import JavaRewrite.MatchResult
import JavaRewrite.Syntax
import JavaRewrite.Traversals
import Data.Functor.Identity (Identity(..), runIdentity)

-- | 'applySubst s e' is the result of replacing each variable present in @s@ with its substitution.
--
-- Example (using testing instances): @applySubst ["a" ~> "foo"] "a + b" = "foo + b"@
applySubst :: Subst -> Exp -> Exp
applySubst subst = go
  where
    -- a single identifier - replace it if it's a metavariable
    go e@(ExpName (Name [name]))
      | Just replacement <- M.lookup name subst
         = replacement
      | otherwise
         = e

    go e | Just (exp, _, setExp) <- fromFieldAccessExp' e
      = setExp (go exp)

    go ExpName{} = error "Redundant pattern, ExpName should be handled by fromFieldAccessExp"

    go e | Just (obj, _, _, args, setObjArgs) <- fromInstanceMethodInvocationExp' e
      = setObjArgs (go obj) (map go args)

    go e@(Assign _ _ _) = e -- FIXME

    go e = mapSubexpressions go e

-- | Map a function over immediate subexpressions of an expression.
mapSubexpressions :: (Exp -> Exp) -> Exp -> Exp
mapSubexpressions go = runIdentity . subexpressions (Identity . go)
