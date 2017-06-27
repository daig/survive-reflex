module Note where
import Reflex.Dom
import Data.Text (Text)

data Note = Note {task :: Text, editing :: Bool}
note :: MonadWidget t m => m a -> m a
note = el "div"
