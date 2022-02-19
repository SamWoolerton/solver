module App.WordsList where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Logic as Logic
import Words as Words

type Input
  = { answer :: Logic.Word }

type State
  = { answer :: Logic.Word }

component :: forall q o m. H.Component q Input o m
component =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval H.defaultEval
    }

render :: forall a cs m. State -> H.ComponentHTML a cs m
render state =
  HH.div_
    [ HH.div_ [ HH.text $ "Answer is " <> state.answer ]
    , HH.div_ [ HH.text $ "Guess is " <> guess ]
    , HH.div_ [ HH.text $ "Score is " <> show scored.score.fully_correct <> " fully correct and " <> show scored.score.partially_correct <> " partially correct." ]
    , HH.div
        [ HP.classes [ HH.ClassName "flex flex-wrap w-full" ]
        ]
        ( map
            (\w -> HH.span [ HP.classes [ HH.ClassName "m-1" ] ] [ HH.text w ])
            $ words_list
        )
    ]
  where
  scored = Logic.score_guess guess state.answer

  words_list = Logic.filter_words scored Words.valid_words

guess ∷ Logic.Word
guess = "clamp"
