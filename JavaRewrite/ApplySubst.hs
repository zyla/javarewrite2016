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

    -- leaves - do nothing to them
    go e@Lit{} = e
    go e@ClassLit{} = e
    go e@This = e
    go e@ThisClass{} = e
    go e@MethodRef{} = e

    -- branches - go through subexpressions
    go e@(InstanceCreation _ _ _ _)
      = mapSubexpressions go e
    go e@(QualInstanceCreation _ _ _ _ _)
      = mapSubexpressions go e
    go (ArrayCreate typ exps dims)
      = ArrayCreate typ (map go exps) dims
    go e@(ArrayCreateInit _ _ _)
      = mapSubexpressions go e
    go e@(FieldAccess _)
      = mapSubexpressions go e

    -- NB: This case is not redundant; a method call without receiver specified
    -- is not handled by @fromInstanceMethodInvocationExp@
    go (MethodInv (MethodCall name args))
      | name == Name [Ident "constant_fold"]
      , [operation] <- args
      , Just res <- constEvalTreeCalc operation -- TODO: error on Nothing?
        = Lit res
      | otherwise = MethodInv (MethodCall name (map go args))

    go (MethodInv (PrimaryMethodCall obj tys name args))
      = error "Redundant pattern, should be handled by fromInstanceMethodInvocationExp"

    go (MethodInv (SuperMethodCall tys name args))
      = MethodInv (SuperMethodCall tys name (map go args))

    go (MethodInv (ClassMethodCall ty tys name args))
      = MethodInv (ClassMethodCall ty tys name (map go args))

    go (MethodInv (TypeMethodCall ty tys name args))
      = MethodInv (TypeMethodCall ty tys name (map go args))

    go (ArrayAccess (ArrayIndex array dims))
      = ArrayAccess (ArrayIndex (go array) (map go dims))

    go (PostIncrement e) = PostIncrement (go e)
    go (PostDecrement e) = PostDecrement (go e)
    go (PreIncrement e) = PreIncrement (go e)
    go (PreDecrement e) = PreDecrement (go e)
    go (PrePlus e) = PrePlus (go e)
    go (PreMinus e) = PreMinus (go e)
    go (PreBitCompl e) = PreBitCompl (go e)
    go (PreNot e) = PreNot (go e)
    go (Cast typ e) = Cast typ (go e)
    go (BinOp e1 op e2) = BinOp (go e1) op (go e2)
    go (InstanceOf e typ) = InstanceOf (go e) typ
    go (Cond cond iffalse iftrue) = Cond (go cond) (go iffalse) (go iftrue)
    go e@(Assign _ _ _) = e -- FIXME
    go e@Lambda{} = mapSubexpressions go e


    constEvalTreeCalc :: Exp -> Maybe Literal
    constEvalTreeCalc (BinOp lhs op rhs) =
      join $ calcFolding op <$> constEvalTreeCalc lhs <*> constEvalTreeCalc rhs
    constEvalTreeCalc (ExpName (Name [name]))
      | Just (Lit a) <- M.lookup name subst = Just a
      | otherwise = Nothing
    constEvalTreeCalc _ = Nothing


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
calcFolding _ Null Null = Just Null
calcFolding _ _ _ = Nothing
