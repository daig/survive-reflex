module Base
  (module X
  ,show',display'
  ,newUnique
  ,switchPromptly'
  ,buttonClass
  ,renderCss
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
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Encoding as T
import qualified Clay as P (render)
import Control.Arrow as X ((&&&))
import Data.Maybe as X
import Control.Monad.Fix as X (mfix)

show' :: Show a => a -> Text
show' = T.pack . show
display' :: (DomBuilder t m, Show a) => a -> m ()
display' = text . show'

newUnique :: MonadIO m => m P.Unique
newUnique = liftIO P.newUnique
instance Show Unique where show = show . P.hashUnique

switchPromptly' :: (Reflex t, MonadHold t m) => Event t (Event t a) -> m (Event t a)
switchPromptly' = switchPromptly never

buttonClass :: MonadWidget t m => Text -> Text -> m (Event t ())
buttonClass t c = do
  (e, _) <- elClass' "button" c $ text t
  return $ domEvent Click e

renderCss = T.encodeUtf8 . LT.toStrict . P.render
