{-# LANGUAGE TupleSections #-}
module Editable where
import Reflex.Dom
import Utils
import Control.Applicative (liftA2)
import Data.Text (Text)
import Data.Semigroup ((<>))

editable :: MonadWidget t m => Event t () -> Bool -> Dynamic t Text -> m (Dynamic t Bool)
editable startEdit initialEditable value = do
  rec finishEdit <- (dyn $ editing <&> \case
                      True -> edit value
                      False -> (el "span" $ text "value: " >> dynText value) &> never)
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
