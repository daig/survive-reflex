module App where
import Base

app :: MonadWidget t m => m ()
app = ul_ $ forM_ messages $ li_ . text


messages :: [Text]
messages = ["Bend","Cheese it"] 
