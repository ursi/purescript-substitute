let testDependencies = [ "assert" ]

in  { name = "substitute"
    , dependencies =
        [ "foreign-object", "mason-prelude", "return" ] # testDependencies
    , packages = ./packages.dhall
    , sources = [ "src/**/*.purs", "test/**/*.purs" ]
    , repository = "https://github.com/ursi/purescript-substitute.git"
    , license = "BSD-3-Clause"
    }
