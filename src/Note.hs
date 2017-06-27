module Note where
import Reflex.Dom
import Data.Text (Text)

type Note = Text
note :: MonadWidget t m => Dynamic t Note -> m (Event t ())
note n = el "div" $ do
  dynText n
  button "x"
