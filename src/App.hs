module App where
import Base
import Data.Unique

app :: MonadWidget t m => m ()
{-app = return ()-}
app = do
  messages <- initMessages
  _ <- ul_ $ listWithKey (constDyn messages) $ \_ v ->
    li_ $ dynText v
  return ()

  

initMessages :: MonadIO m => m (Map Unique Text)
initMessages = liftIO $ foldMap (\x -> newUnique <&> (=: x)) ["Bend","Cheese it"] 
