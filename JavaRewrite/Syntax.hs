-- | Utilities for working with Java syntax
module JavaRewrite.Syntax where

import Language.Java.Syntax
import RandomStuff

-- | The field access expression @a.b@ can be expressed in the AST in two ways
-- (in simplified form):
-- - @ExpName ["a", "b"]@
-- - @FieldAccess (PrimaryFieldAccess (ExpName ["a"], "b"))
--
-- This function converts the two forms to the same representation.
--
-- TODO: what is ClassFieldAccess and what to do about it
fromFieldAccessExp :: Exp -> Maybe (Exp, Ident)
fromFieldAccessExp exp =
  case fromFieldAccessExp' exp of
    Just (exp, field, _) -> Just (exp, field)
    Nothing -> Nothing

-- | Like 'fromFieldAccessExp', but also returns a function to change the
-- expression, with the following property:
--
-- If @fromFieldAccessExp' e = Just (obj, _, setObj)@, then
-- @setObj obj = e@.
--
-- TODO(zyla): document this more, this is becoming a mess.
fromFieldAccessExp' :: Exp -> Maybe (Exp, Ident, Exp -> Exp)

fromFieldAccessExp' (ExpName (Name idents))
  | Just (inner_idents, field_name) <- unsnoc idents
     = Just (ExpName (Name inner_idents), field_name, setExp field_name)
        where
          setExp field_name (ExpName (Name idents)) = ExpName (Name (idents ++ [field_name]))
          setExp field_name exp = FieldAccess (PrimaryFieldAccess exp field_name)

fromFieldAccessExp' (FieldAccess (PrimaryFieldAccess exp field_name))
  = Just (exp, field_name, setExp)
    where
      setExp exp = FieldAccess (PrimaryFieldAccess exp field_name)

fromFieldAccessExp' _ = Nothing

-- | The method invocation expression @a.b()@ can be expressed in the AST in two ways
-- (in simplified form):
-- - @MethodCall ["a", "b"] []@
-- - @PrimaryMethodCall (ExpName ["a"]) [] "b" []@
--
-- This function extracts the receiver of the method, the arguments, generic
-- arguments, method name and a function to replace the values in the original
-- expression, with the property:
--
-- If @fromInstanceMethodInvocationExp e = Just (obj, args, setObjArgs)@, then
-- @setObjArgs obj args = e@ (TODO: check the property).
fromInstanceMethodInvocationExp' :: Exp -> Maybe (Exp, [RefType], Ident, [Argument], Exp -> [Argument] -> Exp)

fromInstanceMethodInvocationExp' (MethodInv (MethodCall (Name idents) args))
  | Just (inner_idents@(_:_), method_name) <- unsnoc idents
    = let obj = ExpName (Name inner_idents)

          setObjArgs (ExpName (Name idents)) args
            = MethodInv (MethodCall (Name (idents ++ [method_name])) args)
          setObjArgs obj args
            = MethodInv (PrimaryMethodCall obj [] method_name args)

      in Just (obj, [], method_name, args, setObjArgs)

fromInstanceMethodInvocationExp' (MethodInv (PrimaryMethodCall obj tys name args))
  = Just (obj, tys, name, args, \obj args -> MethodInv (PrimaryMethodCall obj tys name args))

fromInstanceMethodInvocationExp' _
  = Nothing
