module Utility where

foreign import log2 :: Number -> Number

foreign import json_stringify :: forall a. a -> String
