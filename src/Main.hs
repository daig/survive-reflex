module Main where
import Reflex.Dom (mainWidget')
import Language.Javascript.JSaddle.Warp (run)
import App (app)

main :: IO ()
main = run 8000 $ mainWidget' app
