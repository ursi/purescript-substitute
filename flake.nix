{ inputs =
    { nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
      purs-nix.url ="github:ursi/purs-nix";
      utils.url = "github:ursi/flake-utils/1";
    };

  outputs = { nixpkgs, utils, ... }@inputs:
    utils.default-systems
      ({ make-shell, pkgs, purs-nix, ... }:
         let
           inherit (purs-nix) ps-pkgs ps-pkgs-ns purs;
           package = import ./package.nix purs-nix;

           inherit
             (purs
                { inherit (package) dependencies;
                  test-dependencies = [ ps-pkgs."assert" ps-pkgs-ns.ursi.prelude ];
                  srcs = [ ./src ];
                }
             )
             command;
         in
         { devShell =
             make-shell
               { packages =
                   with pkgs;
                   [ nodejs
                     nodePackages.bower
                     nodePackages.pulp
                     purs-nix.purescript
                     (command { inherit package; })
                   ];
               };
         }
      )
      { inherit inputs nixpkgs; };
}
