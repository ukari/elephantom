module Event.TextInputEvent
  ( textInputEvent
  ) where

import Event.Input
import Event.TextInput (InputText (InputText))
import qualified SDL
import Data.Text.Lazy (fromStrict)

textInputEvent :: SDL.TextInputEventData -> Input
textInputEvent (SDL.TextInputEventData _ t) = TextInput $ InputText (fromStrict t)
