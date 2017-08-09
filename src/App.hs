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
        li_ $ renderNote k v
  return ()

renderNote :: (MonadWidget t m, EventWriter t (Endo Notes) m)
           => Id -> Dynamic t Text -> m ()
renderNote k note = do
  rec
    setEditing <- switchPromptly' <=< dyn $ editing <&> \case
      True -> do
        initText <- sample (current note)
        t <- textInput $ def & textInputConfig_initialValue .~ initText
        let editedValue = (_textInput_value t) `tagPromptlyDyn` keypress Enter t
        tellEvent (setNote k <$> editedValue)
        return $ False <$ editedValue
      False -> do
        (e, clickDelete) <- div' $ do
          span_ $ dynText note
          tellEvent . (Endo (M.delete k) <$) =<< button "x"
        return $ True <$ domEvent Click e
    editing <- foldDyn const False setEditing
  return ()


renderStaticNote :: MonadWidget t m => Dynamic t Text -> m (Event t (),Event t ())
renderStaticNote v = do
  (e,clickDelete) <- div' $ do
    span_ $ dynText v
    button "x"
  return (() <$ domEvent Click e,clickDelete)

{-renderNote :: MonadWidget t m => Dynamic t Text -> m (Event t ())-}
{-renderNote = do-}
  

renderInput :: MonadWidget t m => Id -> Dynamic t Text -> m (TextInput t)
renderInput _ note = do
  initText <- sample (current note)
  textInput $ def & textInputConfig_initialValue .~ initText

type Id = Unique
data Editing = Editing | NotEditing
type Notes = Map Id Text

insNewNote :: MonadIO m => m (Endo Notes)
insNewNote = liftIO $ fmap Endo $ newUnique <&> (`M.insert` "new note")

setNote :: Id -> Text -> Endo Notes
setNote k v = Endo $ M.adjust (\_ -> v) k

getNotes :: MonadIO m => m Notes
getNotes = liftIO $ foldMap (\x -> newUnique <&> (=: x)) ["Bend","Cheese it"] 
