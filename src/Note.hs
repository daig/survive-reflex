module Note where
import Reflex.Dom
import Data.Text (Text)

type Note = Text
note :: MonadWidget t m => Dynamic t Note -> m ()
note = dynText
