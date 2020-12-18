let testDependencies = [ "assert" ]

in  { name = "substitute"
    , dependencies = [ "foreign-object", "mason-prelude" ] # testDependencies
    , packages = ./packages.dhall
    , sources = [ "src/**/*.purs", "test/**/*.purs" ]
    }
