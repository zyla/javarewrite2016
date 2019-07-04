-- | Generic traversals for Java AST.
{-# LANGUAGE DefaultSignatures, FlexibleContexts, FlexibleInstances, TypeOperators,
    DeriveGeneric, MultiParamTypeClasses, UndecidableInstances, ConstraintKinds #-}
module JavaRewrite.Traversals where

import Control.Monad
import GHC.Generics
import Language.Java.Syntax

-- | 'Substructure s a' states that @a@ possibly contains some values of type @s@.
class Substructure s a where
  -- | Traverse the values of type 's' in 'a'.
  substructure :: Applicative f => (s -> f s) -> a -> f a

  default substructure :: (GenericSubstructure s a, Applicative f) => (s -> f s) -> a -> f a
  substructure inj = fmap to . gsubstruct inj . from

type GenericSubstructure s a = (Generic a, GSubstructure s (Rep a))

class GSubstructure s rep where
  gsubstruct :: Applicative f => (s -> f s) -> rep p -> f (rep p)

-- | boilerplate to strip out metadata
instance GSubstructure s a => GSubstructure s (M1 i c a) where
  gsubstruct inj (M1 x) = M1 <$> gsubstruct inj x

instance GSubstructure s U1 where
  gsubstruct inj U1 = pure U1

instance (GSubstructure s a, GSubstructure s b) => GSubstructure s (a :+: b) where
  gsubstruct inj (L1 x) = L1 <$> gsubstruct inj x
  gsubstruct inj (R1 x) = R1 <$> gsubstruct inj x

instance (GSubstructure s a, GSubstructure s b) => GSubstructure s (a :*: b) where
  gsubstruct inj (a :*: b) = (:*:) <$> gsubstruct inj a <*> gsubstruct inj b

instance Substructure s a => GSubstructure s (K1 i a) where
  gsubstruct inj (K1 x) = K1 <$> substructure inj x

-- Base case: Don't recurse further if s is found
instance {-# OVERLAPPING #-} GSubstructure s (K1 i s) where
  gsubstruct inj (K1 x) = K1 <$> inj x

-- Definitions for base types
instance Substructure s Bool where substructure = emptyTraversal
instance Substructure s Char where substructure = emptyTraversal
instance Substructure s Int where substructure = emptyTraversal
instance Substructure s Double where substructure = emptyTraversal
instance Substructure s Integer where substructure = emptyTraversal

instance GSubstructure s (K1 R a) => Substructure s (Maybe a)
instance GSubstructure s (K1 R a) => Substructure s [a]
instance (Substructure s a, Substructure s b) => Substructure s (a, b)

emptyTraversal :: Applicative f => (a -> f a) -> b -> f b
emptyTraversal _ = pure

--------------------------------------------------------------------------------
-- BEGIN MOST BEAUTIFUL CODE IN THIS PROJECT
--------------------------------------------------------------------------------
instance GSubstructure s (Rep Annotation)        => Substructure s Annotation
instance GSubstructure s (Rep ArrayIndex)        => Substructure s ArrayIndex
instance GSubstructure s (Rep ArrayInit)         => Substructure s ArrayInit
instance GSubstructure s (Rep AssignOp)          => Substructure s AssignOp
instance GSubstructure s (Rep BlockStmt)         => Substructure s BlockStmt
instance GSubstructure s (Rep Block)             => Substructure s Block
instance GSubstructure s (Rep Catch)             => Substructure s Catch
instance GSubstructure s (Rep ClassBody)         => Substructure s ClassBody
instance GSubstructure s (Rep ClassDecl)         => Substructure s ClassDecl
instance GSubstructure s (Rep ClassType)         => Substructure s ClassType
instance GSubstructure s (Rep CompilationUnit)   => Substructure s CompilationUnit
instance GSubstructure s (Rep ConstructorBody)   => Substructure s ConstructorBody
instance GSubstructure s (Rep Decl)              => Substructure s Decl
instance GSubstructure s (Rep Diamond)           => Substructure s Diamond
instance GSubstructure s (Rep ElementValue)      => Substructure s ElementValue
instance GSubstructure s (Rep EnumBody)          => Substructure s EnumBody
instance GSubstructure s (Rep EnumConstant)      => Substructure s EnumConstant
instance GSubstructure s (Rep ExplConstrInv)     => Substructure s ExplConstrInv
instance GSubstructure s (Rep Exp)               => Substructure s Exp
instance GSubstructure s (Rep FieldAccess)       => Substructure s FieldAccess
instance GSubstructure s (Rep ForInit)           => Substructure s ForInit
instance GSubstructure s (Rep FormalParam)       => Substructure s FormalParam
instance GSubstructure s (Rep Ident)             => Substructure s Ident
instance GSubstructure s (Rep ImportDecl)        => Substructure s ImportDecl
instance GSubstructure s (Rep InterfaceBody)     => Substructure s InterfaceBody
instance GSubstructure s (Rep InterfaceDecl)     => Substructure s InterfaceDecl
instance GSubstructure s (Rep InterfaceKind)     => Substructure s InterfaceKind
instance GSubstructure s (Rep LambdaExpression)  => Substructure s LambdaExpression
instance GSubstructure s (Rep LambdaParams)      => Substructure s LambdaParams
instance GSubstructure s (Rep Lhs)               => Substructure s Lhs
instance GSubstructure s (Rep Literal)           => Substructure s Literal
instance GSubstructure s (Rep MemberDecl)        => Substructure s MemberDecl
instance GSubstructure s (Rep MethodBody)        => Substructure s MethodBody
instance GSubstructure s (Rep MethodInvocation)  => Substructure s MethodInvocation
instance GSubstructure s (Rep Modifier)          => Substructure s Modifier
instance GSubstructure s (Rep Name)              => Substructure s Name
instance GSubstructure s (Rep Op)                => Substructure s Op
instance GSubstructure s (Rep PackageDecl)       => Substructure s PackageDecl
instance GSubstructure s (Rep PrimType)          => Substructure s PrimType
instance GSubstructure s (Rep RefType)           => Substructure s RefType
instance GSubstructure s (Rep Stmt)              => Substructure s Stmt
instance GSubstructure s (Rep SwitchBlock)       => Substructure s SwitchBlock
instance GSubstructure s (Rep SwitchLabel)       => Substructure s SwitchLabel
instance GSubstructure s (Rep TypeArgument)      => Substructure s TypeArgument
instance GSubstructure s (Rep TypeDeclSpecifier) => Substructure s TypeDeclSpecifier
instance GSubstructure s (Rep TypeDecl)          => Substructure s TypeDecl
instance GSubstructure s (Rep TypeParam)         => Substructure s TypeParam
instance GSubstructure s (Rep Type)              => Substructure s Type
instance GSubstructure s (Rep VarDeclId)         => Substructure s VarDeclId
instance GSubstructure s (Rep VarDecl)           => Substructure s VarDecl
instance GSubstructure s (Rep VarInit)           => Substructure s VarInit
instance GSubstructure s (Rep WildcardBound)     => Substructure s WildcardBound
--------------------------------------------------------------------------------
-- END MOST BEAUTIFUL CODE IN THIS PROJECT
--------------------------------------------------------------------------------

-- | Traversal for expressions contained in a given type,
-- without their subexpressions.
expressions :: (Substructure Exp a, Applicative f) => (Exp -> f Exp) -> a -> f a
expressions = substructure

-- | Traversal for immediate subexpressions for an expression.
subexpressions :: Applicative f => (Exp -> f Exp) -> Exp -> f Exp
subexpressions inj = fmap to . gsubstruct inj . from

-- | 'topdown traversal inj' - apply 'inj' to the value, then recursively to
-- children given by 'traversal'.
--
-- @topdown traversal inj = inj >=> traversal inj >=> (traversal . traversal) inj >=> ...@
topdown :: Monad f => ((a -> f a) -> a -> f a) -> (a -> f a) -> a -> f a
topdown traversal inj = inj >=> traversal (topdown traversal inj)
