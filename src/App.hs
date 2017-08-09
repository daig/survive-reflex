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
  _ <- ul_ $ listWithKey notes $ \_ v ->
    li_ $ dynText v
  return ()

  

type Notes = Map Unique Text

getNotes :: MonadIO m => m Notes
getNotes = liftIO $ foldMap (\x -> newUnique <&> (=: x)) ["Bend","Cheese it"] 
