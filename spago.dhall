{ name = "wordle-mastermind-solver"
, dependencies =
  [ "arrays"
  , "assert"
  , "console"
  , "effect"
  , "functions"
  , "halogen"
  , "integers"
  , "lists"
  , "maybe"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "psci-support"
  , "random"
  , "strings"
  , "stringutils"
  , "tuples"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
