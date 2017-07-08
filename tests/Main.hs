{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec

import Text.Wrap

main :: IO ()
main = hspec $ do
    it "leaves short lines untouched" $ do
      wrapText 5 "foo" `shouldBe` ["foo"]

    it "wraps long lines" $ do
      wrapText 7 "Hello, World!" `shouldBe` ["Hello,", "World!"]

    it "preserves leading whitespace" $ do
      wrapText 10 "  Hello, World!" `shouldBe` ["  Hello,", "World!"]

    it "honors preexisting newlines" $ do
      wrapText 100 "Hello,\nWorld!" `shouldBe` ["Hello,", "World!"]

    it "wraps long lines without truncation" $ do
      wrapText 2 "Hello, World!" `shouldBe` ["Hello,", "World!"]
