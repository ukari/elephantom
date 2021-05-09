module Event.TextInput
  ( InputText (..)
  ) where

import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.ByteString.Lazy.UTF8 (toString)

data InputText = InputText
  { text :: !Text
  } deriving (Eq)

instance Show InputText where
  show (InputText x) = toString $ encodeUtf8 x
