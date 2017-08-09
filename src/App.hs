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
      ul_ $ listWithKey notes renderNote
  return ()

renderNote :: (MonadWidget t m, EventWriter t (Endo Notes) m)
           => Id -> Dynamic t Text -> m ()
renderNote k note = li_ $ do
  rec
    setEditing <- switchPromptly' <=< dyn $ editing <&> \case
      True -> (False <$) <$> renderEdit
      False -> (True <$) <$> renderStaticNote
    editing <- foldDyn const False setEditing
  return ()
  where
    renderEdit = do
      initText <- sample (current note)
      t <- textInput $ def & textInputConfig_initialValue .~ initText
      let editedValue = (_textInput_value t) `tagPromptlyDyn` keypress Enter t
      tellEvent (setNote k <$> editedValue)
      return editedValue
    renderStaticNote = do
        (e, _) <- div' $ do
          span_ $ dynText note
          tellEvent . (Endo (M.delete k) <$) =<< button "x"
        return $ domEvent Click e

type Id = Unique
data Editing = Editing | NotEditing
type Notes = Map Id Text

insNewNote :: MonadIO m => m (Endo Notes)
insNewNote = liftIO $ fmap Endo $ newUnique <&> (`M.insert` "new note")

setNote :: Id -> Text -> Endo Notes
setNote k v = Endo $ M.adjust (\_ -> v) k

getNotes :: MonadIO m => m Notes
getNotes = liftIO $ foldMap (\x -> newUnique <&> (=: x)) ["Bend","Cheese it"] 
