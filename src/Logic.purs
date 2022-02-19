module Logic
  ( CheckedGuess
  , Score
  , Word
  , WordList
  , filter_words
  , is_correct_guess
  , score_guess
  , validate_guess
  ) where

import Prelude
import Data.Array (any, elem, filter, length, mapWithIndex, nub)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains)
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

type Guess
  = { text :: Word, has_dups :: Boolean, chars :: Array String }

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
  processed_guess = preprocess_guess guess

  filter_fn word = case score of
    { fully_correct: 0, partially_correct: 0 } -> not $ any char_in_guess (toCharArray word)
    _ -> (score_guess_impl processed_guess word).score == score

  char_in_guess c = elem c processed_guess.chars

score_guess :: Word -> Word -> CheckedGuess
score_guess guess answer = score_guess_impl (preprocess_guess guess) answer

preprocess_guess :: Word -> Guess
preprocess_guess guess = { text: guess, has_dups, chars: letters }
  where
  letters = (toCharArray guess)

  has_dups = (length letters) == (length $ nub letters)

score_guess_impl :: Guess -> Word -> CheckedGuess
score_guess_impl guess answer = { guess: guess.text, score: { fully_correct, partially_correct } }
  where
  count_true arr = length $ filter identity arr

  fully_correct = count_true $ check_word_exact guess answer

  partially_correct = count_true $ check_word_partial guess answer

check_word_exact :: Guess -> Word -> Array Boolean
check_word_exact guess answer = mapWithIndex (\i c -> check_letter_exact i c answer) guess.chars

check_word_partial :: Guess -> Word -> Array Boolean
check_word_partial guess answer = fn guess answer
  where
  fn = if guess.has_dups then check_word_partial_dups else check_word_partial_no_dups

check_letter_exact :: Int -> String -> Word -> Boolean
check_letter_exact i c answer = Just c == answer_char
  where
  answer_char = charAt i answer

-- TODO:
check_word_partial_dups :: Guess -> Word -> Array Boolean
check_word_partial_dups = check_word_partial_no_dups

check_word_partial_no_dups :: Guess -> Word -> Array Boolean
check_word_partial_no_dups guess answer = mapWithIndex (\i c -> check_letter_partial_no_dups i c answer) guess.chars

check_letter_partial_no_dups :: Int -> String -> Word -> Boolean
check_letter_partial_no_dups i c answer = not exact && contains (Pattern c) answer
  where
  exact = check_letter_exact i c answer
