module Base
  (module X
  ,(<&>)
  ,show',display'
  ,newUnique
  ,switchPromptly'
  ) where

import Reflex.Tags as X
import Data.Map as X (Map)
import Data.Text as X (Text)
import qualified Data.Text as T
import Reflex.Dom.Core as X hiding (tableDynAttr)
import Control.Applicative as X
import Control.Monad as X
import Control.Monad.IO.Class as X (MonadIO(..))
import Data.Monoid as X hiding (Alt)
import qualified Data.Unique as P
import Data.Unique as X (Unique)
import Control.Lens as X hiding (element)

show' :: Show a => a -> Text
show' = T.pack . show
display' :: (DomBuilder t m, Show a) => a -> m ()
display' = text . show'

newUnique :: MonadIO m => m P.Unique
newUnique = liftIO P.newUnique
instance Show Unique where show = show . P.hashUnique

switchPromptly' :: (Reflex t, MonadHold t m) => Event t (Event t a) -> m (Event t a)
switchPromptly' = switchPromptly never
