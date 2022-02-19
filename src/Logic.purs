module Logic where

import Prelude
import Data.Array (any, elem, filter, length, mapWithIndex)
import Data.Maybe (Maybe(..))
import Data.String.Utils (charAt, toCharArray)
import Words as Words

type Word
  = String

type WordList
  = Array Word

type CheckedGuess
  = { guess :: Word
    , score :: Score
    }

type Score
  = { fully_correct :: Int
    , partially_correct :: Int
    }

valid_words :: WordList
valid_words = Words.valid_words

validate_guess :: Word -> Boolean
validate_guess word = correct_length && acceptable_word
  where
  correct_length = length (toCharArray word) == 5

  acceptable_word = elem word Words.all_words

is_correct_guess :: CheckedGuess -> Boolean
is_correct_guess checked_guess = checked_guess.score.fully_correct == 5

filter_words :: CheckedGuess -> WordList -> WordList
filter_words { guess, score } previous_words = filter filter_fn previous_words
  where
  filter_fn w = case score of
    { fully_correct: 0, partially_correct: 0 } -> not $ any char_in_guess (toCharArray w)
    _ -> (score_guess guess w).score == score

  char_in_guess c = elem c $ toCharArray guess

score_guess :: Word -> Word -> CheckedGuess
score_guess guess answer = { guess, score: { fully_correct, partially_correct: 0 } }
  where
  fully_correct = length $ filter identity $ check_word_exact guess answer

check_word_exact :: Word -> Word -> Array Boolean
check_word_exact word answer = mapWithIndex (\i c -> check_letter_exact i c answer) (toCharArray word)

check_letter_exact :: Int -> String -> Word -> Boolean
check_letter_exact i c answer = Just c == answer_char
  where
  answer_char = charAt i answer
