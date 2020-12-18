let testDependencies = [ "assert" ]

in  { name = "my-project"
    , dependencies = [ "foreign-object", "mason-prelude" ] # testDependencies
    , packages = ./packages.dhall
    , sources = [ "src/**/*.purs", "test/**/*.purs" ]
    }
