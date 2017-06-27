{-# LANGUAGE TupleSections #-}
module Editable where
import Reflex.Dom
import Utils
import Control.Applicative (liftA2)
import Data.Text (Text)

editable :: MonadWidget t m => Dynamic t Bool -> Dynamic t Text -> m (Event t ())
editable editing value = 
  (dyn $ editing <&> \editingNow -> 
     (if editingNow then edit value else pure never) <*
     (el "span" $ text "value: " >> dynText value))
  >>= switchPromptly never

edit :: MonadWidget t m => Dynamic t Text -> m (Event t ())
edit value = do
  (div,_) <- el' "div" $ do
    el "span" $ do
      text "edit: "
      dynText value
  return $ () <$ domEvent Click div
