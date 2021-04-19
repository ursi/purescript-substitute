{ ps-pkgs, ... }:
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
  }
