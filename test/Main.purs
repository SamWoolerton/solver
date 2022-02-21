module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Class.Console (log)
import Test.Logic as Logic

main :: Effect Unit
main = do
  log "Logic tests"
  Logic.exact
  Logic.exact
  Logic.exact2
  Logic.unused_duplicate
  Logic.duplicate1
  Logic.duplicate2
  Logic.duplicate3
