module Notes where
import Reflex.Dom ((=:))
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
