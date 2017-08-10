{-# language TemplateHaskell #-}
module Main where
import Reflex.Dom.Main
import Language.Javascript.JSaddle.Warp (run)
import App (app)
import Base (renderCss)
import Clay

main :: IO ()
main = do
  putCss css
  run 8000 $ mainWidgetWithCss (renderCss css) app
  where
    bgColor = "#fdfdfd"
    noteShadow a = boxShadowWithSpread nil nil (em 0.3) (em 0.03) (rgba 0 0 0 a)
    css = do
      body     ? do
        background cornsilk
        fontFamily [] [sansSerif]

      ".add"   ? do
        backgroundColor bgColor
        border solid (px 1) "#ccc"

      ".notes" ? do
        sym margin (em 0.5)
        paddingLeft nil
        maxWidth (em 10)
        listStyleType none

      ".note"  ? do
        overflow auto
        marginBottom (em 0.5)
        sym padding (em 0.5)
        backgroundColor bgColor
        noteShadow 0.3
        ".editable" ? float floatLeft
        ".value"    ? display inlineBlock
        ".delete"   ? do
          float floatRight
          sym padding nil
          backgroundColor bgColor
          borderStyle none
          cursor pointer
          visibility hidden
        hover & do
          noteShadow 0.7
          transitionDuration (sec 0.6)
          ".delete" ? visibility visible
            

