module App where
import Base
import Data.Unique
import qualified Data.Map as M

app :: MonadWidget t m => m ()
{-app = return ()-}
app = do
  initNotes <- getNotes

  addNote <- performEvent . (liftIO newUnique <$) =<< button "+"
  notes <- foldDyn (\k m -> M.insert k "new note" m) initNotes addNote
  _ <- ul_ $ listWithKey notes $ (\_ v -> li_ $ renderNote v)
  return ()

renderNote :: MonadWidget t m => Dynamic t Text -> m ()
renderNote = dynText

type Id = Unique
instance Show Unique where show = show . hashUnique
type Notes = Map Id Text

getNotes :: MonadIO m => m Notes
getNotes = liftIO $ foldMap (\x -> newUnique <&> (=: x)) ["Bend","Cheese it"] 
