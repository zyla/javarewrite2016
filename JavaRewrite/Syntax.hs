-- | Utilities for working with Java syntax
module JavaRewrite.Syntax where

import Language.Java.Syntax
import RandomStuff

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
