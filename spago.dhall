{ name = "purescript-browser-file-system"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "convertable-options"
  , "effect"
  , "exceptions"
  , "foreign"
  , "foreign-object"
  , "maybe"
  , "prelude"
  , "simple-json"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
