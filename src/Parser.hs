module Parser (make) where

import qualified Commands

import Text.Megaparsec.Char
import Data.Text
import Protolude hiding (many, some, any)
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Control.Monad.Combinators


type Parser = Parsec Void Text

make :: Text -> Parser Commands.T
make prefix = string prefix
                *> parseCommand

parseCommand :: Parser Commands.T
parseCommand = choice
                [ parsePlay ]

parsePlay :: Parser Commands.T
parsePlay = do
  _ <- string' "play"
  space1
  song <- pack <$> some anySingle
  return (Commands.Play  song)

parseRemove :: Parser Commands.T
parseRemove = do
  _ <- string' "remove"
  space1
  Commands.Remove <$> Lexer.decimal
