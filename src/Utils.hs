module Utils where
import Reflex.Dom
import Control.Monad.Trans (MonadIO(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Foldable
import Data.Semigroup (Semigroup(..))
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Arrow ((***))

putStrLn' :: MonadIO m => Text -> m ()
putStrLn' t = liftIO $ putStrLn (T.unpack t)

onEvent :: PerformEvent t m => (a -> Performable m b) -> Event t a -> m (Event t b)
onEvent m = performEvent . fmap m

(<&>) :: Functor f => f a -> (a -> b) -> f b
a <&> f = f <$> a
(&>) :: Functor f => f a -> b -> f b
fa &> b = b <$ fa

sampleDyn :: MonadWidget t m => Dynamic t a -> m a
sampleDyn = sample . current

mergeDynFoldableEvents :: (Reflex t, Semigroup a, Foldable f) => Dynamic t (f (Event t a)) -> Event t a
mergeDynFoldableEvents = switchPromptlyDyn . fmap fold

mergeDynFoldableEvents2 :: (Reflex t, Semigroup a, Semigroup b, Foldable f)
                        => Dynamic t (f (Event t a,Event t b)) -> (Event t a, Event t b)
mergeDynFoldableEvents2 = (switchPromptlyDyn *** switchPromptlyDyn) . splitDynPure . fmap fold
