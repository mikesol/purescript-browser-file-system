{ name = "purescript-browser-file-system"
, dependencies =
  [ "effect"
  , "either"
  , "exceptions"
  , "foreign-object"
  , "maybe"
  , "prelude"
  , "record"
  , "tuples"
  , "web-file"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
