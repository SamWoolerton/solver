{ name = "wordle-mastermind-solver"
, dependencies =
  [ "arrays"
  , "console"
  , "effect"
  , "halogen"
  , "maybe"
  , "prelude"
  , "psci-support"
  , "stringutils"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
