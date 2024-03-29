module Notes where
import Utils
import Note
import Editable
import Reflex.Dom
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup (Semigroup((<>)),Endo(..))
import Data.Unique
import Control.Monad.Trans (MonadIO(..))
import Data.Text (Text)

type Id = Unique
type Notes = Map Id Note

newNote :: Note -> IO Notes
newNote n = ffor newUnique (=: n)

notes :: MonadWidget t m => IO Notes -> Event t Notes -> m (Dynamic t Notes)
notes initializeNotes (fmap (Endo . Map.union) -> addNote) = do
  initialNotes <- liftIO initializeNotes
  rec notesDyn <- foldDyn appEndo initialNotes (addNote <> deleteNote)
      (startEdit,deleteNote) <- fmap mergeDynFoldableEvents2 $
        el "ul" $ listWithKey notesDyn $ \i n -> do
          rec (startEdit,deleteNote) <-
                el "li" $ note $ do
                  editing <- editable startEdit False n
                  button "x" <&> (&> Endo (Map.delete i))
          return (startEdit,deleteNote)
  return notesDyn
