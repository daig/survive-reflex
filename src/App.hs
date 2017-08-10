{-# language RecursiveDo #-}
module App where
import Base
import qualified Data.Map as M
import JSDOM.HTMLElement (focus)
{-import GHCJS.DOM.HTMLElement (focus)-}

noteWidget :: MonadWidget t m
           => Dynamic t Text
           -> m (Event t Text -- Entered Text
                ,Event t ())  -- Clicked X
noteWidget note = do
  rec
    (e,r) <- elClass' "div" "note" $ do
      rec
        updateNote <- switchPromptly' <=< dyn $ editing <&> \case
          True -> fmap Just <$> renderEdit
          False -> (Nothing <$ domEvent Click e) <$ renderStaticNote
        editing <- foldDyn const False (isNothing <$> updateNote)
      (fforMaybe updateNote id,) <$> buttonClass "X" "delete"
  return r
  where
    renderStaticNote = elClass "span" "editable value" $ dynText note
    renderEdit = do
      initText <- sample (current note)
      t <- textInput $ def & textInputConfig_initialValue .~ initText
                           & textInputConfig_attributes .~ (constDyn $ "class" =: "editable edit")
      performEvent =<< (focus (_textInput_element t) <$) <$> (delay 0.1 =<< getPostBuild)
      let endEdit = leftmost [keypress Enter t, domEvent Blur t]
      return $ _textInput_value t `tagPromptlyDyn` endEdit

        

app :: MonadWidget t m => m ()
{-app = return ()-}
app = do
  initNotes <- getNotes
  rec
    (notes,updates) <- runEventWriterT $ do
      tellEvent =<< performEvent . (insNewNote <$) =<< buttonClass "+" "add"
      elClass "ul" "notes" $ listWithKey notes $ \k n -> do
        (enterNote,clickDelete) <- noteWidget n
        tellEvent $ Endo (M.delete k) <$ clickDelete
        tellEvent $ Endo . (M.insert k) <$> enterNote
      foldDyn appEndo initNotes updates
  blank

type Id = Unique
data Editing = Editing | NotEditing
type Notes = Map Id Text

insNewNote :: MonadIO m => m (Endo Notes)
insNewNote = liftIO $ fmap Endo $ newUnique <&> (`M.insert` "new note")

setNote :: Id -> Text -> Endo Notes
setNote k v = Endo $ M.adjust (\_ -> v) k

getNotes :: MonadIO m => m Notes
getNotes = liftIO $ foldMap (\x -> newUnique <&> (=: x)) ["Bend","Cheese it"] 
