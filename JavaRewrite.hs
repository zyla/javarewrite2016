module JavaRewrite (
    module JavaRewrite
  , module JavaRewrite.Rule
  , module JavaRewrite.MatchResult
  , module JavaRewrite.ApplySubst
) where

import Data.Monoid
import qualified Data.Set as S

import Language.Java.Syntax

import JavaRewrite.Rule
import JavaRewrite.MatchResult
import JavaRewrite.ApplySubst
import JavaRewrite.Syntax

match :: Pattern -> Exp -> Maybe Subst
match Pattern { pattern_metavars = metavars, pattern_expr = pattern } expr
 = unMatchResult $ matchPattern (S.fromList metavars) pattern expr

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
matchPattern :: S.Set Ident -> Exp -> Exp -> MatchResult
matchPattern metavars = go
  where
    go (ExpName (Name [pname])) exp

      | pname `S.member` metavars
         = singleton pname exp

      | ExpName (Name [ename]) <- exp
      , pname == ename
         = success

    go pattern exp
      | Just (pexp, pfield) <- fromFieldAccessExp pattern
      , Just (eexp, efield) <- fromFieldAccessExp exp
      , pfield == efield
         = go pexp eexp

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
