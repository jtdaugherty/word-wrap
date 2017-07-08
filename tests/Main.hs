{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec

import Text.Wrap

main :: IO ()
main = hspec $ do
    it "leaves short lines untouched" $ do
      wrapTextToLines 5 "foo" `shouldBe` ["foo"]

    it "wraps long lines" $ do
      wrapTextToLines 7 "Hello, World!" `shouldBe` ["Hello,", "World!"]

    it "preserves leading whitespace" $ do
      wrapTextToLines 10 "  Hello, World!" `shouldBe` ["  Hello,", "World!"]

    it "honors preexisting newlines" $ do
      wrapTextToLines 100 "Hello,\nWorld!" `shouldBe` ["Hello,", "World!"]

    it "wraps long lines without truncation" $ do
      wrapTextToLines 2 "Hello, World!" `shouldBe` ["Hello,", "World!"]
