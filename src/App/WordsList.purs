module App.WordsList where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Logic as Logic
import Words as Words

type State
  = { count :: Int }

data Action
  = Increment
  | Decrement

component :: forall q i o m. H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ -> { count: 0 }
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

render :: forall cs m. State -> H.ComponentHTML Action cs m
render state =
  HH.div
    [ HP.classes [ HH.ClassName "flex flex-wrap w-full" ]
    ]
    ( map
        (\w -> HH.span [ HP.classes [ HH.ClassName "m-1" ] ] [ HH.text w ])
        $ words_list
    )

handleAction :: forall cs o m. Action → H.HalogenM State Action cs o m Unit
handleAction = case _ of
  Increment -> H.modify_ \st -> st { count = st.count + 1 }
  Decrement -> H.modify_ \st -> st { count = st.count - 1 }

answer = Logic.correct_word

guess = "clamp"

scored = Logic.score_guess guess answer

words_list = Logic.filter_words scored Words.valid_words
