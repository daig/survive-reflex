{-# language RecursiveDo #-}
module App where
import Base
import qualified Data.Map as M

app :: MonadWidget t m => m ()
{-app = return ()-}
app = do
  initNotes <- getNotes
  addNote <- performEvent . (insNewNote <$) =<< button "+"
  rec
    notes <- foldDyn appEndo initNotes (addNote <> deleteNote)
    (_,deleteNote) <- runEventWriterT $
      ul_ $ listWithKey notes $ \k v ->
        li_ $ renderNote k v >>= tellEvent 
  return ()

renderNote :: MonadWidget t m => Id -> Dynamic t Text -> m (Event t (Endo Notes))
renderNote k v = div_ $ do
  span_ $ dynText v
  (Endo (M.delete k) <$) <$> button "x"

type Id = Unique
type Notes = Map Id Text

insNewNote :: MonadIO m => m (Endo Notes)
insNewNote = liftIO $ Endo <$> newUnique <&> (`M.insert` "new note")

getNotes :: MonadIO m => m Notes
getNotes = liftIO $ foldMap (\x -> newUnique <&> (=: x)) ["Bend","Cheese it"] 
