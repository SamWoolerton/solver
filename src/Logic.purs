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
import Data.Array (any, elem, filter, length, mapWithIndex, nub, unsafeIndex)
import Data.List (List(..), fromFoldable)
import Data.Map.Internal (Map, empty, insertWith, lookup)
import Data.Maybe (Maybe(..), maybe)
import Data.String (Pattern(..), contains)
import Data.String.Utils (charAt, toCharArray)
import Partial.Unsafe (unsafePartial)
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

type Lookup
  = Map String Int

type Answer
  = { text :: Word
    , char_counts :: Lookup
    }

type CharDetails
  = { char :: String, index :: Int, exact :: Boolean }

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

  has_dups = (length letters) /= (length $ nub letters)

score_guess_impl :: Guess -> Word -> CheckedGuess
score_guess_impl guess answer = { guess: guess.text, score: { fully_correct, partially_correct } }
  where
  count_true arr = length $ filter identity arr

  fully_correct_arr = check_word_exact guess answer

  fully_correct = count_true fully_correct_arr

  partially_correct = count_true $ check_word_partial guess answer fully_correct_arr

check_word_exact :: Guess -> Word -> Array Boolean
check_word_exact guess answer = mapWithIndex (\i c -> check_letter_exact i c answer) guess.chars

check_word_partial :: Guess -> Word -> Array Boolean -> Array Boolean
check_word_partial guess answer exact_arr = if guess.has_dups then check_word_partial_dups guess answer exact_arr else check_word_partial_no_dups guess answer

check_letter_exact :: Int -> String -> Word -> Boolean
check_letter_exact i c answer = Just c == answer_char
  where
  answer_char = charAt i answer

check_word_partial_no_dups :: Guess -> Word -> Array Boolean
check_word_partial_no_dups guess answer = mapWithIndex (\i c -> check_letter_partial_no_dups i c answer) guess.chars

check_letter_partial_no_dups :: Int -> String -> Word -> Boolean
check_letter_partial_no_dups i c answer = not exact && letter_in_answer c answer
  where
  exact = check_letter_exact i c answer

letter_in_answer :: String -> Word -> Boolean
letter_in_answer c answer = contains (Pattern c) answer

check_word_partial_dups :: Guess -> Word -> Array Boolean -> Array Boolean
check_word_partial_dups guess answer exact_arr = mapWithIndex (\i c -> check_letter_partial_dups i c answer_processed char_details_arr) guess.chars
  where
  answer_processed = { text: answer, char_counts: frequencies (fromFoldable (toCharArray answer)) empty }

  char_details_arr = mapWithIndex (\i c -> { char: c, index: i, exact: unsafe_index exact_arr i }) guess.chars

-- false means either perfect or not a match, true means partial match
check_letter_partial_dups :: Int -> String -> Answer -> Array CharDetails -> Boolean
check_letter_partial_dups i c answer char_details_arr =
  if char_detail.exact then
    false
  else
    dup_index <= answer_count
  where
  char_detail = unsafe_index char_details_arr i

  exact_matches = length $ filter (\ch -> ch.exact && ch.char == c) char_details_arr

  -- need to remove exact matches of the same letter as they aren't available 
  answer_count = maybe 0 (\x -> x - exact_matches) $ lookup c answer.char_counts

  -- not an exact match, same letter, earlier index
  dup_index = (length $ filter (\ch -> not ch.exact && ch.index < i && ch.char == c) char_details_arr) + 1

unsafe_index :: forall a. Array a -> Int -> a
unsafe_index = unsafePartial $ unsafeIndex

frequencies :: List String -> Map String Int -> Map String Int
frequencies Nil m = m

frequencies (Cons a ls) m = frequencies ls $ increment a m

increment :: String -> Map String Int -> Map String Int
increment key m = insertWith add key 1 m
