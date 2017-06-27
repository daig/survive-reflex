{-# LANGUAGE TupleSections #-}
module Editable where
import Reflex.Dom
import Utils
import Control.Applicative (liftA2)
import Data.Text (Text)
import Data.Semigroup ((<>))

editable :: MonadWidget t m => Event t () -> Bool -> Dynamic t Text -> m (Dynamic t Bool)
editable startEdit initialEditable value = do
  rec finishEdit <- (dyn $ editing <&> \editingNow -> 
                 (if editingNow then edit value else pure never) <*
                 (el "span" $ text "value: " >> dynText value))
                >>= switchPromptly never
      editing <- holdDyn initialEditable $ mergeWith (||) [True <$ startEdit, False <$ finishEdit]
  return editing


edit :: MonadWidget t m => Dynamic t Text -> m (Event t ())
edit value =
   (el "div" $
     el "span" $ do
       text "edit: "
       dynText value)
   &> never
