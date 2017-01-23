{-# OPTIONS_GHC -Werror -Wincomplete-patterns #-}
{-# LANGUAGE FlexibleContexts #-}
module JavaRewrite.ApplySubst where

import qualified Data.Map as M
import Language.Java.Syntax
import JavaRewrite.MatchResult
import JavaRewrite.Syntax
import JavaRewrite.Traversals
import Data.Functor.Identity (Identity(..), runIdentity)
import Control.Monad (join)
import Data.Bits (xor)

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

    -- constant folding
    go (MethodInv (MethodCall name args))
      | name == Name [Ident "constant_fold"]
      , [operation] <- args
      , Just res <- constEvalTreeCalc operation -- TODO: error on Nothing?
        = Lit res
      | otherwise = MethodInv (MethodCall name (map go args))

    go e@(Assign _ _ _) = e -- FIXME

    go e = mapSubexpressions go e

    constEvalTreeCalc :: Exp -> Maybe Literal
    constEvalTreeCalc (BinOp lhs op rhs) =
      join $ calcFolding op <$> constEvalTreeCalc lhs <*> constEvalTreeCalc rhs
    constEvalTreeCalc (ExpName (Name [name]))
      | Just (Lit a) <- M.lookup name subst = Just a
      | otherwise = Nothing
    constEvalTreeCalc _ = Nothing

-- | Map a function over immediate subexpressions of an expression.
mapSubexpressions :: (Exp -> Exp) -> Exp -> Exp
mapSubexpressions go = runIdentity . subexpressions (Identity . go)


calcFolding :: Op -> Literal -> Literal -> Maybe Literal
calcFolding op (Int a) (Int b) =
  case op of
    Add -> Just $ Int (a + b)
    Sub -> Just $ Int (a - b)
    Mult -> Just $ Int (a * b)
    Div -> Just $ Int (a `div` b)
    _ -> Nothing
calcFolding op (Word a) (Word b) =
  case op of
    Add -> Just $ Word (a + b)
    Sub -> Just $ Word (a - b)
    Mult -> Just $ Word (a * b)
    Div -> Just $ Word (a `div` b)
    _ -> Nothing
calcFolding op (Float a) (Float b) =
  case op of
    Add -> Just $ Float (a + b)
    Sub -> Just $ Float (a - b)
    Mult -> Just $ Float (a * b)
    Div -> Just $ Float (a / b)
    _ -> Nothing
calcFolding op (Double a) (Double b) =
  case op of
    Add -> Just $ Double (a + b)
    Sub -> Just $ Double (a - b)
    Mult -> Just $ Double (a * b)
    Div -> Just $ Double (a / b)
    _ -> Nothing
calcFolding op (String a) (String b) =
  case op of
    Add -> Just $ String (a ++ b)
    _ -> Nothing
calcFolding op (Boolean a) (Boolean b) =
  case op of
    CAnd -> Just $ Boolean (a && b)
    COr -> Just $ Boolean (a || b)
    Xor -> Just $ Boolean (a `xor` b)
    _ -> Nothing
calcFolding _ Null Null = Just Null -- FIXME this is an error
calcFolding _ _ _ = Nothing
