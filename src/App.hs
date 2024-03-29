module App where
import Reflex.Dom
import Utils
import Notes
import Control.Monad.Trans (MonadIO(..))
import Data.Text (Text)
import Data.Unique

initializeNotes :: IO Notes
initializeNotes = foldMap newNote ["Learn Reflex", "Do laundry"]

app :: MonadWidget t m => m ()
app = el "div" $ do
  addNote <- button "+" >>= onEvent (\_ -> liftIO $ newNote "New Note")
  _ <- notes initializeNotes addNote
  blank
