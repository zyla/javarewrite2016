module JavaRewrite.Rule where

import Language.Java.Syntax
import Language.Java.Pretty
import Text.PrettyPrint

data Pattern = Pattern { pattern_metavars :: [Ident], pattern_expr :: Exp }
  deriving (Eq, Show)

instance Pretty Pattern where
  pretty (Pattern metavars expr) = ppForall metavars <+> pretty expr
    where
      ppForall [] = empty
      ppForall metavars = text "forall" <+> sep (map pretty metavars) <> text "."

data Rule = Rule { rule_pattern :: Pattern, rule_replacement :: Exp }
  deriving (Eq, Show)

instance Pretty Rule where
  pretty (Rule pattern replacement) =
    pretty pattern <+> text "->" <+> pretty replacement
