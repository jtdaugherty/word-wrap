{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec

import Text.Wrap

main :: IO ()
main = hspec $ do
    it "leaves short lines untouched" $ do
      wrapTextToLines defaultWrapSettings 7 "foo bar"
        `shouldBe` ["foo bar"]

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
      let s = defaultWrapSettings { preserveIndentation = True }
      wrapTextToLines s 10 "  Hello, World!"
        `shouldBe` ["  Hello,", "  World!"]

    it "preserves indentation (2)" $ do
      let s = defaultWrapSettings { preserveIndentation = True }
      wrapTextToLines s 10 "  Hello, World!\n    Things And Stuff"
        `shouldBe` ["  Hello,", "  World!", "    Things", "    And", "    Stuff"]

    it "breaks long non-whitespace tokens" $ do
      let s = defaultWrapSettings { breakLongWords = True }
      wrapTextToLines s 7 "HelloCrazyWorld!\nReallyLong Token"
        `shouldBe` ["HelloCr", "azyWorl", "d!", "ReallyL", "ong", "Token"]

    it "breaks long non-whitespace tokens and indents" $ do
      let s = defaultWrapSettings { breakLongWords = True
                                  , preserveIndentation = True
                                  }
      wrapTextToLines s 7 "  HelloCrazyWorld!\n  ReallyLong Token"
        `shouldBe` [ "  Hello", "  Crazy", "  World", "  !"
                   , "  Reall", "  yLong", "  Token"
                   ]

    it "gracefully handles indentation longer than the target width when breaking is off" $ do
      let s = defaultWrapSettings { breakLongWords = False
                                  , preserveIndentation = True
                                  }
      wrapTextToLines s 4 "           foo bar"
        `shouldBe` ["   foo", "   bar"]

    it "gracefully handles indentation longer than the target width when breaking is on" $ do
      let s = defaultWrapSettings { breakLongWords = True
                                  , preserveIndentation = True
                                  }
      wrapTextToLines s 4 "           foo bar"
        `shouldBe` ["   f", "   o", "   o", "   b", "   a", "   r"]
