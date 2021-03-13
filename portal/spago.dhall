{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "accessibility"
, dependencies =
  [ "aff-coroutines"
  , "affjax"
  , "argonaut"
  , "bigints"
  , "console"
  , "coroutines"
  , "effect"
  , "filterable"
  , "halogen"
  , "http-methods"
  , "newtype"
  , "numbers"
  , "openlayers"
  , "psci-support"
  , "routing"
  , "routing-duplex"
  , "random"
  , "parsing"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
