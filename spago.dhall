{ name = "wordle-mastermind-solver"
, dependencies =
  [ "arrays"
  , "console"
  , "effect"
  , "halogen"
  , "maybe"
  , "prelude"
  , "psci-support"
  , "random"
  , "stringutils"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
