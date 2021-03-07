module Lib (
  prependDistributeTry,
  tryIfConsumes,
  ConsumeStatus (..),
) where

import Relude
import Text.Megaparsec (MonadParsec (lookAhead, try), Parsec)

type MyParsec = Parsec Void Text

doNotConsume :: MyParsec a -> MyParsec a
doNotConsume = try . lookAhead

data ParsingStatus = Fails | Succeeds
  deriving stock (Eq, Show)

data ConsumeStatus = Consumes | DoesNotConsume
  deriving stock (Eq, Show)

tryIfFails :: MyParsec a -> MyParsec ParsingStatus
tryIfFails p = do
  (try p >> return Succeeds) <|> return Fails

tryIfConsumes :: MyParsec a -> MyParsec ConsumeStatus
tryIfConsumes p =
  do
    try ((p >> return Consumes) <|> return DoesNotConsume)
    <|> return Consumes

-- Applies `try` on the left operand and distributes the `try` over the
-- monoidal operation.
--
-- For example `prependDistributeTry a b` will only consume input on failure if
-- b consumes input.
--
-- Plain `try left >> right` wouldn't work. Once `left` succeeds, it is consumed
-- without backtracking.
prependDistributeTry :: MyParsec a -> MyParsec b -> MyParsec b
prependDistributeTry left right = do
  leftResult <- lookAhead $ tryIfFails left
  case leftResult of
    Fails -> left >> right
    Succeeds -> do
      rightResult <- doNotConsume $ left >> tryIfConsumes right
      case rightResult of
        Consumes -> left >> right
        DoesNotConsume -> try (left >> right)
