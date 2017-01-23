module JavaRewrite.ConstantFold where

import Data.Bits (xor)

import Language.Java.Syntax
import Language.Java.Pretty

import JavaRewrite.Types
import JavaRewrite.Traversals

-- | Perform constant folding on subexpressions annotated with @constant_fold(e)@.
evalConstantFoldMacros :: Exp -> Either RewriteError Exp
evalConstantFoldMacros = topdown subexpressions go
  where
    -- constant folding
    go (MethodInv (MethodCall (Name [Ident "constant_fold"]) [expr]))
       = case constEvalTreeCalc expr of
           Nothing -> Left $ ConstantFoldingFailed $ "Expression: " ++ show expr
           Just result -> pure (Lit result)

    go e = pure e

constEvalTreeCalc :: Exp -> Maybe Literal
constEvalTreeCalc (Lit lit) = Just lit
constEvalTreeCalc (BinOp lhs op rhs) = do
  lhs' <- constEvalTreeCalc lhs
  rhs' <- constEvalTreeCalc rhs
  calcFolding op lhs' rhs'
constEvalTreeCalc _ = Nothing

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
