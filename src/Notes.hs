module Notes where
import Utils
import Note
import Editable
import Reflex.Dom
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup ((<>),Endo(..))
import Data.Unique
import Control.Monad.Trans (MonadIO(..))
import Data.Text (Text)

type Id = Unique
type Notes = Map Id Note

newNote :: Text -> IO Notes
newNote t = ffor newUnique (=: Note t True)

notes :: MonadWidget t m => IO Notes -> Event t Notes -> m (Dynamic t Notes)
notes initializeNotes (fmap (Endo . Map.union) -> addNote) = do
  initialNotes <- liftIO initializeNotes
  rec notesDyn <- foldDyn appEndo initialNotes (addNote <> deleteNote)
      deleteNote <- fmap mergeDynFoldableEvents $
        el "ul" $ listWithKey notesDyn $ \i n ->
          el "li" $ note $ do
            editable (n <&> editing) (n <&> task)
            button "x" <&> (&> Endo (Map.delete i))
  return notesDyn
