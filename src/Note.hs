module Note where
import Reflex.Dom
import Data.Text (Text)

type Note = Text
note :: MonadWidget t m => m a -> m a
note = el "div"
