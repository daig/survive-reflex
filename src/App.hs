module App where
import Reflex.Dom
import Notes

app :: MonadWidget t m => m ()
app = notes
