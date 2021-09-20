module Parser
  ( make
  ) where

import qualified Commands

import           Control.Monad.Combinators
import           Data.Text
import           Protolude               hiding ( any
                                                , many
                                                , some
                                                )
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as Lexer


type Parser = Parsec Void Text

make :: Text -> Parser Commands.T
make prefix = string prefix *> parseCommand

parseCommand :: Parser Commands.T
parseCommand = choice [parsePlay]

parsePlay :: Parser Commands.T
parsePlay = do
  _ <- string' "play"
  space1
  song <- pack <$> some anySingle
  return (Commands.Play song)

parseRemove :: Parser Commands.T
parseRemove = do
  _ <- string' "remove"
  space1
  Commands.Remove <$> Lexer.decimal

parseSkip :: Parser Commands.T
parseSkip = Commands.Skip <$ string' "skip"
