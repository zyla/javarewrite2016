module JavaRewrite.Rule where

import Language.Java.Syntax
import Language.Java.Pretty
import Text.PrettyPrint

-- | Possible restrictions on the values of metavariables.
data ExpressionType
    = IntLiteral
    | WordLiteral
    | FloatLiteral
    | DoubleLiteral
    | BooleanLiteral
    | CharLiteral
    | StringLiteral
    | NullLiteral
    deriving (Eq, Show, Read, Ord, Enum, Bounded)

instance Pretty ExpressionType where
  pretty = text . show

data Metavariable = Metavariable { metavar_name :: Ident, metavar_type :: Maybe ExpressionType }
  deriving (Eq, Show, Ord)

instance Pretty Metavariable where
  pretty (Metavariable ident Nothing) = pretty ident
  pretty (Metavariable ident (Just ty)) = parens (pretty ident <+> text ":" <+> pretty ty)

data Pattern = Pattern { pattern_metavars :: [Metavariable], pattern_expr :: Exp }
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
