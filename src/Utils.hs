module Utils where
import Reflex.Dom
import Control.Monad.Trans (MonadIO(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Foldable
import Data.Semigroup (Semigroup(..))
import Data.Map (Map)
import qualified Data.Map as Map

putStrLn' :: MonadIO m => Text -> m ()
putStrLn' t = liftIO $ putStrLn (T.unpack t)

onEvent :: PerformEvent t m => (a -> Performable m b) -> Event t a -> m (Event t b)
onEvent m = performEvent . fmap m

mergeDynFoldableEvents :: (Reflex t, Semigroup a, Foldable f) => Dynamic t (f (Event t a)) -> Event t a
mergeDynFoldableEvents = switchPromptlyDyn . fmap fold
