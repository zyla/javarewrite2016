> module Rewrite where
> import Data.List as List

The expression type we're operating on. Supports variables, integer literals and function application.

    expr ::= <integer>     -- Lit
           | <identifier>  -- Var
           | expr expr     -- App

Patterns additionally support "metavariables".

    pattern ::= expr
              | '$' <identifier> -- MetaVar

For simplicity, I cram both into one data type. MetaVar should not appear in plain Exprs.

Sorry for bad error handling.

> data Expr = Lit Int | Var String | App Expr Expr | MetaVar String deriving Show
> type Pattern = Expr

The matching function takes a pattern and an expression, and returns a list of
metavariable substitutions if the match was successful.

Note: non-linear patterns[1] are not handled correctly (result in duplicate substitutions).

[1]: patterns where a metavariable appears more than once.

> match :: Pattern -> Expr -> Maybe [(String, Expr)]

First the easy case: a metavariable matches anything and creates a substitution.

> match (MetaVar var) expr = Just [(var, expr)]

Each constructor matches itself.

> match (Lit a) (Lit b) | a == b = Just []
> match (Var a) (Var b) | a == b = Just []
>
> match (App fa xa) (App fb xb) =
>   (++) <$> match fa fb <*> match xa xb

MetaVars in plain Exprs are invalid.

> match _ (MetaVar _) = error "MetaVar in expression"

Anything else does not match.

> match _ _ = Nothing

Next, we need a way to apply the resulting substitutions.

`applySubsts substs expr` replaces occurences of `MetaVar`s mentioned in `substs` with corresponding replacements.

Note: is is not valid to have an undefined MetaVar in the expression.

> applySubsts :: [(String, Expr)] -> Expr -> Expr

MetaVars are just replaced.

> applySubsts substs (MetaVar var)
>   = case List.lookup var substs of
>       Just val -> val
>       Nothing  -> error $ "Unknown MetaVar: " ++ var

Anything that has a nested Expr must be handled recursively.

> applySubsts substs (App f x) = App (applySubsts substs f) (applySubsts substs x)

Leaf expressions are just copied.

> applySubsts _ expr = expr

Rewriting (at the top level) is just combining these two.

> rewrite :: Pattern -> Expr -> Expr -> Expr
> rewrite lhs rhs expr
>   = case match lhs expr of
>       Just substs -> applySubsts substs rhs
>       Nothing     -> expr

> -- forall f x y. flip f x y = f y x
> exampleRuleLHS = foldl1 App [Var "flip", MetaVar "f", MetaVar "x", MetaVar "y"]
> exampleRuleRHS = foldl1 App [MetaVar "f", MetaVar "y", MetaVar "x"]
> exampleExpr = foldl1 App [Var "flip", Var "add", Lit 1, Lit 5]
> exampleResult = rewrite exampleRuleLHS exampleRuleRHS exampleExpr
> -- App (App (Var "add") (Lit 5)) (Lit 1)
