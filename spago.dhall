{ name = "wordle-mastermind-solver"
, dependencies =
  [ "arrays"
  , "assert"
  , "console"
  , "effect"
  , "functions"
  , "halogen"
  , "lists"
  , "maybe"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "psci-support"
  , "random"
  , "strings"
  , "stringutils"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
