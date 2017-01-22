-- | Generic traversals for Java AST.
{-# LANGUAGE DefaultSignatures, FlexibleContexts, FlexibleInstances, TypeOperators,
    DeriveGeneric, MultiParamTypeClasses, UndecidableInstances #-}
module JavaRewrite.Traversals where

import GHC.Generics
import Language.Java.Syntax

-- | 'Substructure s a' states that @a@ possibly contains some values of type @s@.
class Substructure s a where
  -- | Traverse the values of type 's' in 'a'.
  substructure :: Applicative f => (s -> f s) -> a -> f a

  default substructure :: (Generic a, GSubstructure s (Rep a), Applicative f) => (s -> f s) -> a -> f a
  substructure inj = fmap to . gsubstruct inj . from

class GSubstructure s rep where
  gsubstruct :: Applicative f => (s -> f s) -> rep p -> f (rep p)

-- | boilerplate to strip out metadata
instance GSubstructure Exp a => GSubstructure Exp (M1 i c a) where
  gsubstruct inj (M1 x) = M1 <$> gsubstruct inj x

instance GSubstructure Exp U1 where
  gsubstruct inj U1 = pure U1

instance (GSubstructure Exp a, GSubstructure Exp b) => GSubstructure Exp (a :+: b) where
  gsubstruct inj (L1 x) = L1 <$> gsubstruct inj x
  gsubstruct inj (R1 x) = R1 <$> gsubstruct inj x

instance (GSubstructure Exp a, GSubstructure Exp b) => GSubstructure Exp (a :*: b) where
  gsubstruct inj (a :*: b) = (:*:) <$> gsubstruct inj a <*> gsubstruct inj b

instance Substructure Exp a => GSubstructure Exp (K1 i a) where
  gsubstruct inj (K1 x) = K1 <$> substructure inj x

-- Base case: Don't recurse further if Exp is found
instance {-# OVERLAPPING #-} GSubstructure Exp (K1 i Exp) where
  gsubstruct inj (K1 x) = K1 <$> inj x

-- Definitions for base types
instance Substructure Exp Bool where substructure = emptyTraversal
instance Substructure Exp Char where substructure = emptyTraversal

instance GSubstructure Exp (K1 R a) => Substructure Exp (Maybe a)
instance GSubstructure Exp (K1 R a) => Substructure Exp [a]
instance (Substructure Exp a, Substructure Exp b) => Substructure Exp (a, b)

emptyTraversal :: Applicative f => (a -> f a) -> b -> f b
emptyTraversal _ = pure

--------------------------------------------------------------------------------
-- BEGIN MOST BEAUTIFUL CODE IN THIS PROJECT
--------------------------------------------------------------------------------
instance Substructure Exp Annotation
instance Substructure Exp ArrayInit
instance Substructure Exp Block
instance Substructure Exp BlockStmt
instance Substructure Exp Catch
instance Substructure Exp ClassBody
instance Substructure Exp ClassDecl
instance Substructure Exp ClassType
instance Substructure Exp CompilationUnit
instance Substructure Exp ConstructorBody
instance Substructure Exp Decl
instance Substructure Exp ElementValue
instance Substructure Exp EnumBody
instance Substructure Exp EnumConstant
instance Substructure Exp ExplConstrInv
instance Substructure Exp ForInit
instance Substructure Exp FormalParam
instance Substructure Exp Ident
instance Substructure Exp ImportDecl
instance Substructure Exp InterfaceBody
instance Substructure Exp InterfaceDecl
instance Substructure Exp InterfaceKind
instance Substructure Exp MemberDecl
instance Substructure Exp MethodBody
instance Substructure Exp Modifier
instance Substructure Exp Name
instance Substructure Exp PackageDecl
instance Substructure Exp PrimType
instance Substructure Exp RefType
instance Substructure Exp Stmt
instance Substructure Exp SwitchBlock
instance Substructure Exp SwitchLabel
instance Substructure Exp Type
instance Substructure Exp TypeArgument
instance Substructure Exp TypeDecl
instance Substructure Exp TypeParam
instance Substructure Exp VarDecl
instance Substructure Exp VarDeclId
instance Substructure Exp VarInit
instance Substructure Exp WildcardBound
--------------------------------------------------------------------------------
-- END MOST BEAUTIFUL CODE IN THIS PROJECT
--------------------------------------------------------------------------------

-- | Traversal for expressions contained in a given type,
-- without their subexpressions.
expressions :: (Substructure Exp a, Applicative f) => (Exp -> f Exp) -> a -> f a
expressions = substructure
