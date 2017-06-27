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

newNote :: Note -> IO Notes
newNote n = ffor newUnique (=: n)

notes :: MonadWidget t m => IO Notes -> Event t Notes -> m (Dynamic t Notes)
notes initializeNotes addNote = do
  initialNotes <- liftIO initializeNotes
  notesDyn <- foldDyn Map.union initialNotes addNote
  _ <- el "ul" $ list notesDyn $ \note ->
         el "li" $ dynText note
  return notesDyn
