module Note where
import Utils
import Reflex.Dom
import Data.Text (Text)

type Note = Text
note :: MonadWidget t m => m a -> m (Event t (),a)
note children = el' "div" children <&> \(div,a) ->
  (() <$ domEvent Click div,a)
