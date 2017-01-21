module JavaRewrite where

import Data.Monoid
import qualified Data.Map as M
import qualified Data.Set as S
import Language.Java.Syntax
import JavaRewrite.Rule

import Test.QuickCheck

type Subst = M.Map Ident Exp

match :: Pattern -> Exp -> Maybe Subst
match Pattern { pattern_metavars = metavars, pattern_expr = pattern } expr
 = unMatchResult $ matchPattern (S.fromList metavars) pattern expr

newtype MatchResult = MatchResult { unMatchResult :: Maybe Subst } deriving (Eq, Show)

instance Monoid MatchResult where
  mempty = MatchResult $ Just M.empty
  MatchResult a `mappend` MatchResult b = MatchResult (M.union <$> a <*> b)

singleton :: Ident -> Exp -> MatchResult
singleton k v = MatchResult $ Just $ M.singleton k v

failure :: MatchResult
failure = MatchResult Nothing

success :: MatchResult
success = mempty

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

-- The field access expression @a.b@ can be expressed in the AST in two ways
-- (in simplified form):
-- - @ExpName ["a", "b"]@
-- - @FieldAccess (PrimaryFieldAccess (ExpName ["a"], "b"))
--
-- This function converts the two forms to the same representation.
--
-- TODO: what is ClassFieldAccess and what to do about it
fromFieldAccessExp :: Exp -> Maybe (Exp, Ident)
fromFieldAccessExp (ExpName (Name idents))
  | Just (inner_idents, field_name) <- unsnoc idents
  = Just (ExpName (Name inner_idents), field_name)
fromFieldAccessExp (FieldAccess (PrimaryFieldAccess exp field_name))
  = Just (exp, field_name)
fromFieldAccessExp _ = Nothing

unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc [x] = Just ([], x)
unsnoc (x : xs) = (\(init, tail) -> (x : init, tail)) <$> unsnoc xs

prop_unsnoc_append :: [Int] -> Int -> Property
prop_unsnoc_append xs y = unsnoc (xs ++ [y]) === Just (xs, y)
