module JavaRewrite.Rule where

import Language.Java.Syntax
import Language.Java.Pretty
import Text.PrettyPrint

data Rule = Rule { rule_metavars :: [Ident], rule_pattern :: Exp, rule_replacement :: Exp }
  deriving (Eq, Show)

instance Pretty Rule where
  pretty (Rule metavars pattern replacement) =
    ppForall metavars <+> pretty pattern <+> text "->" <+> pretty replacement

ppForall [] = empty
ppForall metavars = text "forall" <+> sep (map pretty metavars) <> text "."
