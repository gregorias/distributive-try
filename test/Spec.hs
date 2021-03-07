{-# LANGUAGE OverloadedStrings #-}

module Spec (main) where

import Lib (
  ConsumeStatus (DoesNotConsume),
  prependDistributeTry,
  tryIfConsumes,
 )
import Relude (
  Alternative (many, (<|>)),
  Either (Right),
  IO,
  Maybe (Just, Nothing),
  Monad (return, (>>)),
  void,
  ($),
 )
import Test.Hspec (
  SpecWith,
  describe,
  hspec,
  it,
  shouldBe,
 )
import Text.Megaparsec (
  MonadParsec (try),
  anySingle,
  parse,
  parseMaybe,
 )
import Text.Megaparsec.Char (
  spaceChar,
  string,
 )

main :: IO ()
main = hspec tests

tests :: SpecWith ()
tests = do
  describe "tryIfConsumes" $ do
    it "Returns DoesNotConsume if the parser doesn't consume." $ do
      let prefixRestP = do
            result <- tryIfConsumes (string "prefix" >> string "rest")
            void $ many anySingle
            return result
      parseMaybe prefixRestP "prefrest" `shouldBe` Just DoesNotConsume

  describe "prependDistributeTry" $ do
    it "does not consume input if right doesn't consume" $
      do
        let followUpP = try (string "prefix") >> string "rest"
        let composedP = prependDistributeTry spaceChar followUpP
        parse (composedP <|> string " noprefix") "" " noprefix"
          `shouldBe` Right " noprefix"
    it "consumes input if left and right consume" $
      do
        let followUpP = try (string "prefix") >> string "rest"
        let composedP = prependDistributeTry spaceChar followUpP
        parseMaybe (composedP <|> string " prefixnorest") " prefixnorest"
          `shouldBe` Nothing
