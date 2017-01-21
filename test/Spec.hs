{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Monoid

import Test.Hspec
import Test.QuickCheck
import TestUtils

import RandomStuff
import JavaRewrite

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

    it "is a commutative monoid" $
      property $ \(x :: MatchResult) y -> do
        x <> y `shouldBe` y <> x

    it "merges disjoint substitutions" $ do
      singleton "a" "1 + 1" <> singleton "b" "2 + 2"
        `shouldBe` MatchResult (Just ["a" ~> "1 + 1", "b" ~> "2 + 2"])

    it "merges substitutions which agree on common variables" $ do
      MatchResult (Just ["a" ~> "1 + 1", "b" ~> "2 + 2"])
          <> MatchResult (Just ["a" ~> "1 + 1", "c" ~> "1"])
        `shouldBe`
          MatchResult (Just
            [ "a" ~> "1 + 1"
            , "b" ~> "2 + 2"
            , "c" ~> "1" ])

    it "conflict in substitutions results in a failure" $ do
      singleton "a" "1 + 1" <> singleton "a" "2 + 2" `shouldBe` failure

  describe "unsnoc" $
    it "works" $
      property $ \(xs :: [Int]) (y :: Int) -> 
        unsnoc (xs ++ [y]) === Just (xs, y)

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
      property $ forAll (genExp 5) $ \e ->
        match (Pattern [] e) e == Just []

    -- TODO test several Exp constructors

    it "handles nonlinear patterns" $ do
      match "forall a. a + a" "1 + 1" `shouldBe` Just ["a" ~> "1"]
      match "forall a. a + a" "1 + 2" `shouldBe` Nothing
