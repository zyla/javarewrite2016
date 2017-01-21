module JavaRewrite.MatchResult where

import Language.Java.Syntax
import qualified Data.Map as M

type Subst = M.Map Ident Exp

-- | A result of matching a 'Pattern' against an 'Exp'.
-- Can be either 'failure' or a substitution ('Subst') of free variables from
-- the 'Pattern'.
--
-- It is a 'Monoid'. The monoidal operation ('mappend') represents merging
-- results of matching subexpressions of a given expression. 'mempty' is a
-- success with an empty substitution.
--
-- Failure in any of the subexpressions means failure of the whole match, so
-- 'failure' is an absorbing element of 'mappend'.
--
-- When the substitutions resulting from matching two subexpressions are
-- disjoint (that is, the subexpressions in the pattern don't share any
-- metavariables), the result of merging them is simply their union.
--
-- If they do have common metavariables (the pattern is nonlinear), the result
-- becomes a failure if they don't agree on one of the metavariables.
newtype MatchResult = MatchResult { unMatchResult :: Maybe Subst } deriving (Eq, Show)

instance Monoid MatchResult where
  mempty = MatchResult (Just M.empty)
  MatchResult a `mappend` MatchResult b =
      MatchResult (bindM2 mergeMaps a b)

    where

      bindM2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
      bindM2 f ma mb = do a <- ma; b <- mb; f a b
      
      -- | 'mergeMaps m1 m2' is @Just (union m1 m2)@ if the maps have the same values for common keys,
      -- and @Nothing@ otherwise.
      mergeMaps :: (Ord k, Eq v) => M.Map k v -> M.Map k v -> Maybe (M.Map k v)
      mergeMaps s1 s2 = sequence $ M.unionWith (bindM2 keepIfEqual) (fmap Just s1) (fmap Just s2)

      keepIfEqual :: Eq a => a -> a -> Maybe a
      keepIfEqual e1 e2
        | e1 == e2  = Just e1
        | otherwise = Nothing

singleton :: Ident -> Exp -> MatchResult
singleton k v = MatchResult $ Just $ M.singleton k v

failure :: MatchResult
failure = MatchResult Nothing

success :: MatchResult
success = mempty
