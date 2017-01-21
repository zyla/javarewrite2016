{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad
import Data.Monoid
import Data.String

import Test.Hspec
import Test.QuickCheck

import Language.Java.Syntax
import qualified Language.Java.Lexer as Lexer
import qualified Language.Java.Parser as Parser

import JavaRewrite
import JavaRewrite.Rule
import qualified JavaRewrite.RuleParser as Parser

main = hspec $ do
  describe "MatchResult" $ do
    it "propagates failure properly" $
      property $ \(x :: MatchResult) -> do
        failure <> x `shouldBe` failure
        x <> failure `shouldBe` failure

    it "respects Monoid law 1" $
      property $ \(x :: MatchResult) -> do
        mempty <> x `shouldBe` x
        x <> mempty `shouldBe` x

    it "respects Monoid law 2" $
      property $ \(x :: MatchResult) y z -> do
        x <> (y <> z) `shouldBe` (x <> y) <> z

    -- TODO non-linear patterns

  describe "unsnoc" $
    it "works" $
      property prop_unsnoc_append

  describe "matchPattern" $ do
    it "recognizes metavariables properly" $ do
      match "forall a. a" "foo" `shouldBe` Just ["a" ~> "foo"]
      match "forall a. b" "foo" `shouldBe` Nothing

    it "recognizes literals properly" $ do
      match "forall a. 1 + a" "1 + 2" `shouldBe` Just ["a" ~> "2"]
      match "forall a. \"asd\" + a" "\"asd\" + 2" `shouldBe` Just ["a" ~> "2"]
      match "forall a. 1 + a" "2 + 2" `shouldBe` Nothing

    it "recognizes pre/post increment/decrement" $ do
      match "forall a. a++" "foo++" `shouldBe` Just ["a" ~> "foo"]
      match "forall a. ++a" "++foo" `shouldBe` Just ["a" ~> "foo"]
      match "forall a. a--" "foo--" `shouldBe` Just ["a" ~> "foo"]
      match "forall a. --a" "--foo" `shouldBe` Just ["a" ~> "foo"]

    it "recognizes pre operators" $ do
      match "forall a. +a" "+foo" `shouldBe` Just ["a" ~> "foo"]
      match "forall a. -a" "-foo" `shouldBe` Just ["a" ~> "foo"]
      match "forall a. !a" "!foo" `shouldBe` Just ["a" ~> "foo"]
      match "forall a. ~a" "~foo" `shouldBe` Just ["a" ~> "foo"]

    it "recognizes cast operation" $ do
      match "forall a. (String)a" "(String)foo" `shouldBe` Just ["a" ~> "foo"]
      match "forall a. (Int)a" "(String)foo" `shouldBe` Nothing
      -- match "forall a b. (b)a" "(String)foo" `shouldBe` Just ["a" ~> "foo", "b" ~> "String"]

    it "recognizes instanceof operation" $ do
      match "forall a. a instanceof String" "foo instanceof String"
        `shouldBe` Just ["a" ~> "foo"]
      -- match "forall a b. a instanceof b" "foo instanceof String"
      -- `shouldBe` Just ["a" ~> "foo", "b" ~> "String"]

    it "recognizes condition operator" $ do
      match "forall a b c. a?b:c" "foo==bar?1:2" `shouldBe`
        Just ["a" ~> "foo==bar", "b" ~> "1", "c" ~> "2"]

    it "recognizes 'this' keyword" $ do
      match "forall a. this" "this" `shouldBe` Just []

    it "recognizes array creation" $ do
      match "forall a. new boolean[a][][]" "new boolean[true][][]" `shouldBe`
        Just ["a" ~> "true"]
      match "forall a. new boolean[a][][]" "new boolean[true][]" `shouldBe`
        Nothing
      match "forall a. new boolean[false][][]" "new boolean[true][][]"
        `shouldBe` Nothing

    it "handles different number of dimensions in array creation" $ do
      match "new boolean[1]" "new boolean[1][1]" `shouldBe` Nothing

    it "matches ExpName with itself" $ do
      match "foo" "foo" `shouldBe` Just []

    it "matches abritrary expression with itself" $
      property $ forAll (genExp 5) $ \(e :: Exp) ->
        match (Pattern [] e) e == Just []

    -- TODO test several Exp constructors


-------------------------------------------------------------------------------
-- utilities

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
