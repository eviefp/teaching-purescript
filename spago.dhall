{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "console"
  , "effect"
  , "psci-support"
  , "node-readline"
  , "node-readline-aff"
  , "quickcheck"
  , "node-fs"
  , "simple-json"
  , "aff"
  , "affjax"
  , "argonaut"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
