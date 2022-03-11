let conf = ./spago.dhall

in      conf
    //  { sources = conf.sources # [ "example/**/*.purs" ]
        , dependencies =
              conf.dependencies
            # [ "halogen"
              , "halogen-storybook"
              , "simple-json"
              , "either"
              , "variant"
              , "filterable"
              , "foldable-traversable"
              , "arrays"
              , "transformers"
              , "tuples"
              , "web-file"
              ]
        }
