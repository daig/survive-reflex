{-# LANGUAGE FlexibleContexts #-}
module Utils where
import Reflex.Dom
import Control.Monad.Trans (MonadIO(..))
import Data.Text (Text)
import qualified Data.Text as T

putStrLn' :: MonadIO m => Text -> m ()
putStrLn' t = liftIO $ putStrLn (T.unpack t)

onEvent :: PerformEvent t m => (a -> Performable m b) -> Event t a -> m (Event t b)
onEvent m = performEvent . fmap m
