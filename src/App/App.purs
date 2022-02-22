module App.App where

import Prelude
import Data.Array (catMaybes, last, length, slice, snoc, sortBy)
import Data.Entropy as Entropy
import Data.Map (lookup)
import Data.Maybe (Maybe(..), maybe)
import Data.String.CodeUnits as StringCodeUnits
import Data.Tuple (Tuple(..), fst, snd)
import Data.Words as Words
import Halogen as H
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Logic as Logic
import Web.UIEvent.KeyboardEvent (key)

type Input
  = { answer :: Logic.Word }

type State
  = { answer :: Logic.Word
    , guess :: String
    , validation_errors :: Maybe String
    , guesses :: Array ({ guess :: Logic.CheckedGuess, filtered :: Array Entropy.Entropy })
    }

data Action
  = InputEntered String
  | SubmitGuess String

component :: forall q o m. H.Component q Input o m
component =
  H.mkComponent
    { initialState:
        \{ answer } ->
          { answer
          , guess: ""
          , validation_errors: Nothing
          , guesses: []
          }
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
            , HH.div_
                [ HH.input
                    [ HP.type_ HP.InputText
                    , HE.onValueInput InputEntered
                    -- not sure if there's a better way to do a no-op
                    , HE.onKeyDown \e -> if (key e) == "Enter" then SubmitGuess state.guess else InputEntered state.guess
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
                ]
            , case state.validation_errors of
                Nothing -> HH.div_ []
                Just message -> HH.div [ HP.classes [ HH.ClassName "my-3 px-4 py-2 inline-block bg-red-100 text-red-800" ] ] [ HH.text message ]
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
                $ (if count_possibilities > 30 then "Showing the top 30 of " else "")
                <> (show count_possibilities)
                <> " possibilities"
                <> ( if count_possibilities == count_all then
                      ""
                    else
                      (", out of a total " <> (show count_all) <> " words")
                  )
            , HH.div
                [ HP.classes [ HH.ClassName "w-full" ]
                ]
                ( map
                    ( \(Tuple guess entropy) ->
                        HH.div
                          [ HE.onClick \_ -> SubmitGuess guess
                          , HP.classes [ HH.ClassName "my-1 px-3 py-2 cursor-pointer hover:bg-gray-200 shadow-md flex items-center" ]
                          ]
                          [ HH.div [ HP.classes [ HH.ClassName "w-12 mr-2" ] ] [ HH.text guess ]
                          , HH.div
                              [ HP.classes [ HH.ClassName "h-4" ]
                              , HP.style ("background-color: hsl(216deg " <> (show (entropy * 0.6 + 15.0)) <> "% 50%); width: " <> (show entropy) <> "%;")
                              ]
                              []
                          ]
                    )
                    $ slice 0 30 words_list
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
  words_list = sortBy (\a b -> compare (snd b) (snd a)) $ maybe (Logic.normalise_entropy Entropy.entropy_arr) (\m -> m.filtered) (last state.guesses)

  count_possibilities = length words_list

  count_all = length Words.valid_words

  wrong_length = (StringCodeUnits.length state.guess) /= 5

handleAction :: forall output m. Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  InputEntered g -> H.modify_ \st -> st { guess = g }
  SubmitGuess g ->
    H.modify_ \st -> case Logic.validate_guess g of
      Nothing -> handle_step $ st { guess = g, validation_errors = Nothing }
      Just message -> st { guess = g, validation_errors = Just message }

handle_step :: State -> State
handle_step st = st { guess = "", guesses = guesses }
  where
  checked = Logic.score_guess st.guess st.answer

  words = maybe Entropy.entropy_arr (\m -> m.filtered) (last st.guesses)

  filtered = Logic.filter_words checked $ map fst words

  with_entropy =
    -- use pre-calculated entropy figures
    if length filtered > 60 then
      catMaybes $ map (\w -> (\e -> Tuple w e) <$> (lookup w Entropy.entropy_map)) filtered
    else
      -- few enough to run this synchronously
      Logic.calculate_entropy filtered

  guesses = snoc st.guesses { guess: checked, filtered: Logic.normalise_entropy with_entropy }

heading ∷ ∀ (t1 ∷ Type) (t2 ∷ Type). String → HTML t1 t2
heading text = HH.h3 [ HP.classes [ HH.ClassName "mb-2" ] ] [ HH.text text ]

subheading ∷ ∀ (t1 ∷ Type) (t2 ∷ Type). String → HTML t1 t2
subheading text =
  HH.div [ HP.classes [ HH.ClassName "text-gray-600 text-sm mb-2" ] ]
    [ HH.text text ]
