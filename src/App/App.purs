module App.App where

import Prelude
import Data.Array (last, length, snoc)
import Data.Maybe (maybe)
import Data.String.CodeUnits as StringCodeUnits
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
    [ HH.div [ HP.classes [ HH.ClassName "flex flex-wrap px-4 py-2" ] ]
        [ HH.div [ HP.classes [ HH.ClassName "w-full md:w-1/2 mb-4" ] ]
            [ HH.h3 [ HP.classes [ HH.ClassName "mb-2" ] ] [ HH.text "Guess the mystery 5-letter word" ]
            , HH.input
                [ HP.type_ HP.InputText
                , HE.onValueInput InputEntered
                , HP.value state.guess
                , HP.required true
                , HP.classes [ HH.ClassName "bg-gray-100 p-2 mr-3" ]
                ]
            , HH.button
                [ HE.onClick \_ -> SubmitGuess
                , HP.disabled wrong_length
                , HP.classes [ HH.ClassName $ "text-white px-3 py-2 " <> if wrong_length then "bg-gray-400" else "bg-gray-700" ]
                ]
                [ HH.text $ "Check guess" ]
            , HH.div_
                ( map
                    ( \x ->
                        HH.div [ HP.classes [ HH.ClassName "mt-3 flex" ] ]
                          [ HH.div [ HP.classes [ HH.ClassName "px-3 py-1" ] ] [ HH.text x.guess.guess ]
                          , HH.div [ HP.classes [ HH.ClassName "px-3 py-1 bg-green-200" ] ] [ HH.text $ show x.guess.score.fully_correct ]
                          , HH.div [ HP.classes [ HH.ClassName "px-3 py-1 bg-orange-200" ] ] [ HH.text $ show x.guess.score.partially_correct ]
                          ]
                    )
                    $ state.guesses
                )
            ]
        , HH.div [ HP.classes [ HH.ClassName "w-full md:w-1/2" ] ]
            [ HH.h3_ [ HH.text "Remaining possible words" ]
            , HH.div [ HP.classes [ HH.ClassName "text-gray-600 text-sm mb-2" ] ]
                [ HH.text $ if count_possibilities == count_all then (show count_possibilities) <> " possibilities" else ((show count_possibilities) <> " possibilities, out of a total " <> (show count_all) <> " words") ]
            , HH.div
                [ HP.classes [ HH.ClassName "flex flex-wrap w-full" ]
                ]
                ( map
                    (\w -> HH.span [ HP.classes [ HH.ClassName "m-1" ] ] [ HH.text w ])
                    $ words_list
                )
            ]
        ]
    , HH.div [ HP.classes [ HH.ClassName "p-2 text-center" ] ]
        [ HH.text "Credit for the game mode at "
        , HH.a [ HP.href "https://playhurdle.vercel.app/", HP.classes [ HH.ClassName "text-blue-700 font-bold" ] ] [ HH.text "Hurdle" ]
        , HH.text ", and how to estimate entropy from "
        , HH.a [ HP.href "https://www.youtube.com/watch?v=v68zYyaEmEA", HP.classes [ HH.ClassName "text-blue-700 font-bold" ] ] [ HH.text "3Blue1Brown" ]
        ]
    ]
  where
  words_list = maybe Words.valid_words (\m -> m.filtered) (last state.guesses)

  count_possibilities = length words_list

  count_all = length Words.valid_words

  wrong_length = (StringCodeUnits.length state.guess) /= 5

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
