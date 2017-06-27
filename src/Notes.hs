module Notes where
import Note
import Reflex.Dom
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup ((<>))
import Data.Unique
import Control.Monad.Trans (MonadIO(..))

type Id = Unique
type Notes = Map Id Note

newNote :: Note -> IO Notes
newNote n = ffor newUnique (=: n)

notes :: MonadWidget t m => IO Notes -> Event t Notes -> m (Dynamic t Notes)
notes initializeNotes addNote = do
  initialNotes <- liftIO initializeNotes
  notesDyn <- foldDyn Map.union initialNotes addNote
  _ <- el "ul" $ list notesDyn $ \n ->
         el "li" $ note n
  return notesDyn
