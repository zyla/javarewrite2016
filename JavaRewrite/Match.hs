module JavaRewrite.Match where

import Data.Monoid
import qualified Data.Map.Strict as M

import JavaRewrite.Rule
import JavaRewrite.MatchResult
import JavaRewrite.Syntax
import Language.Java.Syntax

match :: Pattern -> Exp -> Maybe Subst
match Pattern { pattern_metavars = metavars, pattern_expr = pattern } expr
 = unMatchResult $ matchPattern context pattern expr
  where
    context = M.fromList $ map (\(Metavariable ident ty) -> (ident, ty)) metavars

type MetavariableContext = M.Map Ident (Maybe ExpressionType)

{-
Missing Exp pattern matches:
 - FieldAccess
 - ClassLit
 - ThisClass
 - InstanceCreation
 - QualInstanceCreation
 - ArrayCreateInit
 - MethodInv
 - ArrayAccess
 - Assign
 - Lambda
 - MethodRef
-}
matchPattern :: MetavariableContext -> Exp -> Exp -> MatchResult
matchPattern metavars = go
  where
    go (ExpName (Name [pname])) exp

      | Just maybeExprType <- M.lookup pname metavars
         = if exprMatchesType maybeExprType exp
             then singleton pname exp
             else failure

      | ExpName (Name [ename]) <- exp
      , pname == ename
         = success

    go pattern exp
      | Just (pexp, pfield) <- fromFieldAccessExp pattern
      , Just (eexp, efield) <- fromFieldAccessExp exp
      , pfield == efield
         = go pexp eexp

      | Just (pobj, ptys, pname, pargs, _) <- fromInstanceMethodInvocationExp' pattern
      , Just (eobj, etys, ename, eargs, _) <- fromInstanceMethodInvocationExp' exp
      , ptys == etys
      , pname == ename
      , length pargs == length eargs
         = go pobj eobj <> mconcat (zipWith go pargs eargs)

    go (MethodInv (MethodCall pname pargs)) (MethodInv (MethodCall ename eargs))
      | pname == ename
         = mconcat (zipWith go pargs eargs)

    go (BinOp pl pop pr) (BinOp el eop er)
      | pop == eop
         = go pl el <> go pr er

    -- Lit match
    go (Lit pl) (Lit el)
      | pl == el = success

    -- Pre/Post Increment/Decrement match
    go (PostIncrement pe) (PostIncrement ee)
      = go pe ee

    go (PreIncrement pe) (PreIncrement ee)
      = go pe ee

    go (PostDecrement pe) (PostDecrement ee)
      = go pe ee

    go (PreDecrement pe) (PreDecrement ee)
      = go pe ee

    -- Pre operators match
    go (PrePlus pe) (PrePlus ee)
      = go pe ee

    go (PreMinus pe) (PreMinus ee)
      = go pe ee

    go (PreBitCompl pe) (PreBitCompl ee)
      = go pe ee

    go (PreNot pe) (PreNot ee)
      = go pe ee

    -- Cast match
    go (Cast ptype pexp) (Cast etype eexp)
      | ptype == etype = go pexp eexp

    -- InstanceOf match
    go (InstanceOf pexp preftype) (InstanceOf eexp ereftype)
      | preftype == ereftype = go pexp eexp

    -- Conditional operator match
    go (Cond pbool ptrueexp pfalseexp) (Cond ebool etrueexp efalseexp)
      = go pbool ebool <> go ptrueexp etrueexp <> go pfalseexp efalseexp

    -- Array match
    go (ArrayCreate ptype pexps pdim) (ArrayCreate etype eexps edim)
      | ptype == etype
      , pdim == edim
      , length pexps == length eexps
         = mconcat (zipWith go pexps eexps)

    go This This = success

    go _ _ = failure


exprMatchesType :: Maybe ExpressionType -> Exp -> Bool
exprMatchesType (Just IntLiteral)     (Lit Int{}) = True
exprMatchesType (Just WordLiteral)    (Lit Word{}) = True
exprMatchesType (Just FloatLiteral)   (Lit Float{}) = True
exprMatchesType (Just DoubleLiteral)  (Lit Double{}) = True
exprMatchesType (Just BooleanLiteral) (Lit Boolean{}) = True
exprMatchesType (Just CharLiteral)    (Lit Char{}) = True
exprMatchesType (Just StringLiteral)  (Lit String{}) = True
exprMatchesType (Just NullLiteral)    (Lit Null) = True
exprMatchesType Nothing _ = True
exprMatchesType _ _ = False
