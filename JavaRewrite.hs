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

newtype MatchResult = MatchResult { unMatchResult :: Maybe Subst }

instance Monoid MatchResult where
  mempty = MatchResult $ Just M.empty
  MatchResult a `mappend` MatchResult b = MatchResult (M.union <$> a <*> b)

singleton :: Ident -> Exp -> MatchResult
singleton k v = MatchResult $ Just $ M.singleton k v

failure :: MatchResult
failure = MatchResult Nothing

matchPattern :: S.Set Ident -> Exp -> Exp -> MatchResult
matchPattern metavars = go
  where
    go (ExpName (Name [name])) exp
      | name `S.member` metavars
         = singleton name exp

    go pattern exp
      | Just (pexp, pfield) <- fromFieldAccessExp pattern
      , Just (eexp, efield) <- fromFieldAccessExp exp
      , pfield == efield
         = go pexp eexp

    go (BinOp pl pop pr) (BinOp el eop er)
      | pop == eop
         = go pl el <> go pr er

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
