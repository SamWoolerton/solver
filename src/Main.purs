module Main where

import Prelude
-- import App.AppMain as AppMain
-- import App.Input as AppMain
import App.WordsList as AppMain
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

-- import Words
main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI AppMain.component { answer: "crust" } body
