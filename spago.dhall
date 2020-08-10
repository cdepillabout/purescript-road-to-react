{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, dependencies =
  [ "affjax"
  , "console"
  , "effect"
  , "js-timers"
  , "profunctor-lenses"
  , "psci-support"
  , "react-basic"
  , "react-basic-dom"
  , "react-basic-hooks"
  , "record"
  ]
}
