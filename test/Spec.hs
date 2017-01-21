{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Main where

import Data.Monoid
import Data.String

import Test.Hspec
import Test.QuickCheck (property)

import Language.Java.Syntax
import qualified Language.Java.Lexer as Lexer
import qualified Language.Java.Parser as Parser

import JavaRewrite
import JavaRewrite.Rule
import qualified JavaRewrite.RuleParser as Parser

main = hspec $ do
  describe "MatchResult" $ do
    it "propagates failure properly" $ do
      failure <> success `shouldBe` failure
      success <> failure `shouldBe` failure

    -- TODO non-linear patterns

  describe "unsnoc" $
    it "works" $
      property prop_unsnoc_append

  describe "matchPattern" $ do
    it "recognizes metavariables properly" $ do
      match "forall a. a" "foo" `shouldBe` Just ["a" ~> "foo"]
      match "forall a. b" "foo" `shouldBe` Nothing

    it "matches expression with itself" $ do
      match "foo" "foo" `shouldBe` Just []
      -- TODO generalize the above using QuickCheck
      -- forall (e :: Exp). match (Pattern [] e) e == Just []

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
