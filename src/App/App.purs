module App.App where

import Prelude
import Data.Array (last, snoc)
import Data.Maybe (maybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Logic as Logic
import Words as Words

type Input
  = { answer :: Logic.Word }

type State
  = { answer :: Logic.Word, guess :: String, guesses :: Array ({ guess :: Logic.CheckedGuess, filtered :: Logic.WordList }) }

data Action
  = InputEntered String
  | SubmitGuess

component :: forall q o m. H.Component q Input o m
component =
  H.mkComponent
    { initialState: \{ answer } -> { answer, guess: "", guesses: [] }
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div_
    [ HH.input
        [ HP.type_ HP.InputText
        , HE.onValueInput InputEntered
        , HP.value state.guess
        , HP.required true
        ]
    , HH.button [ HE.onClick \_ -> SubmitGuess ] [ HH.text "Check guess" ]
    , HH.div_ [ HH.text $ "Answer is " <> state.answer ]
    , HH.div_
        ( map
            ( \x ->
                HH.div_
                  [ HH.span_ [ HH.text x.guess.guess ]
                  , HH.span_ [ HH.text $ show x.guess.score.fully_correct ]
                  , HH.span_ [ HH.text $ show x.guess.score.partially_correct ]
                  ]
            )
            $ state.guesses
        )
    , HH.div
        [ HP.classes [ HH.ClassName "flex flex-wrap w-full" ]
        ]
        ( map
            (\w -> HH.span [ HP.classes [ HH.ClassName "m-1" ] ] [ HH.text w ])
            $ words_list
        )
    ]
  where
  words_list = maybe Words.valid_words (\m -> m.filtered) (last state.guesses)

handleAction :: forall output m. Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  InputEntered g -> H.modify_ \st -> st { guess = g }
  SubmitGuess -> H.modify_ \st -> if Logic.validate_guess st.guess then handle_step st else st

handle_step :: State -> State
handle_step st = st { guess = "", guesses = guesses }
  where
  checked = Logic.score_guess st.guess st.answer

  words = maybe Words.valid_words (\m -> m.filtered) (last st.guesses)

  filtered = Logic.filter_words checked words

  guesses = snoc st.guesses { guess: checked, filtered }
