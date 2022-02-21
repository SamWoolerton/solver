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
  | SubmitGuess String

component :: forall q o m. H.Component q Input o m
component =
  H.mkComponent
    { initialState: \{ answer } -> { answer, guess: "", guesses: [] }
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div [ HP.classes [ HH.ClassName "container mx-auto" ] ]
    [ HH.div [ HP.classes [ HH.ClassName "flex flex-wrap px-4 py-2" ] ]
        [ HH.div [ HP.classes [ HH.ClassName "w-full md:w-1/2 mb-4" ] ]
            [ heading "Guess the mystery 5-letter word"
            , subheading "Enter a guess below or click one of the options"
            , HH.input
                [ HP.type_ HP.InputText
                , HE.onValueInput InputEntered
                , HP.value state.guess
                , HP.required true
                , HP.classes [ HH.ClassName "bg-gray-100 p-2 mr-3" ]
                ]
            , HH.button
                [ HE.onClick \_ -> SubmitGuess state.guess
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
            [ heading "Remaining possible words"
            , subheading
                $ (show count_possibilities)
                <> " possibilities"
                <> ( if count_possibilities == count_all then
                      ""
                    else
                      ("out of a total " <> (show count_all) <> " words")
                  )
            , HH.div
                [ HP.classes [ HH.ClassName "flex flex-wrap w-full -mx-1" ]
                ]
                ( map
                    ( \w ->
                        HH.span
                          [ HE.onClick \_ -> SubmitGuess w
                          , HP.classes [ HH.ClassName "m-1 cursor-pointer hover:bg-gray-200" ]
                          ]
                          [ HH.text w ]
                    )
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
  SubmitGuess g -> H.modify_ \st -> if Logic.validate_guess g then handle_step $ st { guess = g } else st

handle_step :: State -> State
handle_step st = st { guess = "", guesses = guesses }
  where
  checked = Logic.score_guess st.guess st.answer

  words = maybe Words.valid_words (\m -> m.filtered) (last st.guesses)

  filtered = Logic.filter_words checked words

  guesses = snoc st.guesses { guess: checked, filtered }

heading text = HH.h3 [ HP.classes [ HH.ClassName "mb-2" ] ] [ HH.text text ]

subheading text =
  HH.div [ HP.classes [ HH.ClassName "text-gray-600 text-sm mb-2" ] ]
    [ HH.text text ]
