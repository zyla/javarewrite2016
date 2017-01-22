module TestUtils where

import Control.Monad

import Test.QuickCheck
import Data.String

import Language.Java.Syntax
import qualified Language.Java.Lexer as Lexer
import qualified Language.Java.Parser as Parser

import JavaRewrite
import JavaRewrite.Rule
import qualified JavaRewrite.RuleParser as Parser

(~>) = (,)

-- | Instance only for tests.
instance IsString Exp where
  fromString = unsafeParse Parser.exp

-- | Instance only for tests.
instance IsString Ident where
  fromString = Ident

-- | Instance only for tests.
instance IsString Pattern where
  fromString = unsafeParse Parser.pattern

-- | Instance only for tests.
instance IsString Rule where
  fromString = unsafeParse Parser.rule

-- | Instance only for tests.
instance IsString CompilationUnit where
  fromString = unsafeParse Parser.compilationUnit

unsafeParse :: Parser.P a -> String -> a
unsafeParse p input =
  case Parser.parse (p <* Parser.eof) "<input>" $ Lexer.lexer input of
    Left err -> error (show err)
    Right val -> val

instance Arbitrary MatchResult where
  arbitrary = MatchResult <$> arbitrary

instance Arbitrary Exp where
  arbitrary = genExp 3

-- | Generate an arbitrary 'Exp' with given maximum depth.
genExp :: Int {- depth -} -> Gen Exp
genExp depth = oneof (branches depth ++ leaves)
  where
    branches :: Int -> [Gen Exp]
    branches 0 = []
    branches _ =
      [ FieldAccess <$> (PrimaryFieldAccess <$> genExpNext <*> arbitrary)
      -- , ClassLit <$> arbitrary
      -- , ThisClass <$> arbitrary
      -- , InstanceCreation
      -- , QualInstanceCreation
      -- , ArrayCreate Type [Exp] Int
      -- , ArrayCreateInit Type Int ArrayInit
      -- , FieldAccess <$> (ClassFieldAccess ...)
      -- , MethodInv MethodInvocation
      -- , ArrayAccess ArrayIndex
      , PostIncrement <$> genExpNext
      , PostDecrement <$> genExpNext
      , PreIncrement <$> genExpNext
      , PreDecrement <$> genExpNext
      , PrePlus <$> genExpNext
      , PreMinus <$> genExpNext
      , PreBitCompl <$> genExpNext
      , PreNot <$> genExpNext
      , Cast <$> arbitrary <*> genExpNext
      , BinOp <$> genExpNext <*> arbitrary <*> genExpNext
      , InstanceOf <$> genExpNext <*> arbitrary
      , Cond <$> genExpNext <*> genExpNext <*> genExpNext
      -- , Assign <$> arbitrary <*> arbitrary <$> genExpNext
      -- , Lambda LambdaParams LambdaExpression
      -- , MethodRef Name Ident
      ]
      where
        genExpNext = genExp (depth - 1)
    
    leaves = 
      [ Lit <$> arbitrary
      , pure This
      , ExpName <$> arbitrary
      ]

instance Arbitrary Literal where
  arbitrary = oneof
    [ Int <$> arbitrary
    , Word <$> arbitrary
    , Float <$> arbitrary
    , Double <$> arbitrary
    , Char <$> arbitrary
    , String <$> arbitrary
    , pure Null
    ]

instance Arbitrary Type where
  arbitrary = oneof
    [ PrimType <$> arbitraryBoundedEnum
    , RefType <$> arbitrary
    ]

instance Arbitrary RefType where
  arbitrary = oneof
    [ ClassRefType <$> arbitrary
    -- TODO disabled for now, because it's recursive
    -- , ArrayType <$> arbitrary
    ]

instance Arbitrary ClassType where
  arbitrary = ClassType <$> listOf1 ((,) <$> arbitrary <*> pure [])
    -- TODO lol generics

instance Arbitrary Op where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary AssignOp where
  arbitrary = arbitraryBoundedEnum

-- The following instances stolen from language-java tests/Tests.hs

instance Arbitrary Name where
    arbitrary = Name <$> (choose (1,3) >>= \len -> replicateM len arbitrary)
instance Arbitrary Ident where
    arbitrary = Ident <$> (choose (1,15) >>= \len -> replicateM len (elements (['a'..'z'] ++ ['A'..'Z'])))
