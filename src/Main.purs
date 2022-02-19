module Main where

import Prelude
import App.App as App
import Effect (Effect)
import Effect.Random (randomInt)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Data.Array (length, unsafeIndex)
import Partial.Unsafe (unsafePartial)
import Words as Words
import Logic as Logic

main :: Effect Unit
main = do
  answer <- get_random_word
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI App.component { answer } body

get_random_word :: Effect Logic.Word
get_random_word = do
  random_int <- randomInt 0 max_words_index
  pure $ get_at_index random_int
  where
  max_words_index = (length Words.valid_words) - 1

  get_at_index n = unsafePartial $ unsafeIndex Words.valid_words n
