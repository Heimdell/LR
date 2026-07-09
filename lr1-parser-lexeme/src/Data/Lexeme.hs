module Data.Lexeme where

import Data.String (IsString(fromString))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Position (Pos)

data Payload
  = LowercaseName Text
  | UppercaseName Text
  | NumberLiteral Integer
  | StringLiteral Text
  | Operator      Text
  | Punctuator    Text
  | Reserved      Text
  deriving stock (Eq)

instance Show Payload where
  show = \case
    LowercaseName _ -> "<name>"
    UppercaseName _ -> "<Name>"
    NumberLiteral _ -> "<num>"
    StringLiteral _ -> "<str>"
    Operator      _ -> "<op>"
    Punctuator    _ -> "<pun>"
    Reserved      n -> Text.unpack n

type Lexeme = (Pos, Payload)

instance {-# OVERLAPS #-} Show Lexeme where
  show = show . snd

instance IsString Payload where
  fromString = Reserved . fromString
