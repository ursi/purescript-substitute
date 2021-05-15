{ ps-pkgs, licenses, ... }:
  with ps-pkgs;
  { version = "0.2.1";

    dependencies =
      [ foldable-traversable
        foreign-object
        maybe
        prelude
        return
        strings
      ];

    pursuit =
      { name = "substitute";
        repo = "https://github.com/ursi/purescript-substitute.git";
        license = licenses.bsd3;
      };
  }
