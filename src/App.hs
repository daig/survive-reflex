module App where
import Base
import Notes

app :: MonadWidget t m => m ()
app = do
  rec (_,clickAdd) <- notesWidget getNotes ("new note" <$ clickAdd)
  blank

getNotes :: IO Notes
getNotes = (`foldMap` ["Bend","Cheese it"]) $ \note -> newUnique <&> (=: note)
