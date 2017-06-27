module Notes where
import Reflex.Dom
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup ((<>))

type Id = Int
type Note = Text
type Notes = Map Id Note

initialNotes :: Notes
initialNotes = 0 =: "Learn Reflex"
            <> 1 =: "Do laundry"

notes :: MonadWidget t m => m ()
notes = (() <$) $ el "ul" $
  list (constDyn initialNotes) $ \note ->
    el "li" $ dynText note
