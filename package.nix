{ ps-pkgs, ps-pkgs-ns, ... }:
  with ps-pkgs;
  { version = "0.2.0";

    dependencies =
      [ foreign-object
        ps-pkgs-ns.ursi.prelude
        return
      ];
  }
