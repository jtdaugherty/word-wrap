{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec

import Text.Wrap

main :: IO ()
main = hspec $ do
    it "leaves short lines untouched" $ do
      wrapTextToLines defaultWrapSettings 5 "foo"
        `shouldBe` ["foo"]

    it "wraps long lines" $ do
      wrapTextToLines defaultWrapSettings 7 "Hello, World!"
        `shouldBe` ["Hello,", "World!"]

    it "preserves leading whitespace" $ do
      wrapTextToLines defaultWrapSettings 10 "  Hello, World!"
        `shouldBe` ["  Hello,", "World!"]

    it "honors preexisting newlines" $ do
      wrapTextToLines defaultWrapSettings 100 "Hello,\n\n \nWorld!"
        `shouldBe` ["Hello,", "", "", "World!"]

    it "wraps long lines without truncation" $ do
      wrapTextToLines defaultWrapSettings 2 "Hello, World!"
        `shouldBe` ["Hello,", "World!"]

    it "preserves indentation" $ do
      let s = WrapSettings { preserveIndentation = True }
      wrapTextToLines s 10 "  Hello, World!"
        `shouldBe` ["  Hello,", "  World!"]

    it "preserves indentation (2)" $ do
      let s = WrapSettings { preserveIndentation = True }
      wrapTextToLines s 10 "  Hello, World!\n    Things And Stuff"
        `shouldBe` ["  Hello,", "  World!", "    Things", "    And", "    Stuff"]
