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
  , "datetime-iso"
  , "effect"
  , "generics-rep"
  , "halogen"
  , "http-methods"
  , "numbers"
  , "psci-support"
  , "routing"
  , "routing-duplex"
  , "uuid"
  , "openlayers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
