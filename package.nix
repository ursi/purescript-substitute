{ ps-pkgs, ... }:
  with ps-pkgs;
  { version = "0.2.0";

    dependencies =
      [ foldable-traversable
        foreign-object
        maybe
        prelude
        return
        strings
      ];
  }
