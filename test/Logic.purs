module Test.Logic where

import Prelude
import Effect (Effect)
import Logic as L
import Test.Assert (assertEqual)

exact = check "guess" "guess" 5 0

exact2 = check "creed" "bleed" 3 0

unused_duplicate = check "guess" "trace" 0 1

duplicate1 = check "beech" "crest" 1 1

duplicate2 = check "crest" "trees" 2 2

duplicate3 = check "creed" "pleat" 1 0

check ∷ L.Word → L.Word → Int → Int → Effect Unit
check guess answer fully_correct partially_correct =
  assertEqual
    { expected: L.score_guess guess answer
    , actual: { guess, score: { fully_correct, partially_correct } }
    }
