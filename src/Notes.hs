module Notes where
import Reflex.Dom
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup ((<>))
import Data.Unique
import Control.Monad.Trans (MonadIO(..))

type Id = Unique
type Note = Text
type Notes = Map Id Note

initializeNotes :: IO Notes
initializeNotes = foldMap newNote ["Learn Reflex", "Do laundry"]
  where newNote n = ffor newUnique (=: n)

notes :: MonadWidget t m => m ()
notes = do
  initialNotes <- constDyn <$> liftIO initializeNotes
  (() <$) $ el "ul" $
    list initialNotes $ \note ->
      el "li" $ dynText note
