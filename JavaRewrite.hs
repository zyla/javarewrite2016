module JavaRewrite where

import Data.Monoid
import qualified Data.Map as M
import qualified Data.Set as S
import Language.Java.Syntax
import JavaRewrite.Rule

type Subst = M.Map Ident Exp

match :: Rule -> Exp -> Maybe Subst
match Rule { rule_metavars = metavars, rule_pattern = pattern } expr
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
      | name `S.member` metavars = singleton name exp
    go (BinOp pl pop pr) (BinOp el eop er)
      | pop == eop = go pl el <> go pr er
    go _ _ = failure
