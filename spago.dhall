{ name = "wordle-mastermind-solver"
, dependencies =
  [ "arrays"
  , "console"
  , "effect"
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
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
