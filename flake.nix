{ inputs =
    { nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
      purs-nix.url = "github:ursi/purs-nix";
      utils.url = "github:ursi/flake-utils";
    };

  outputs = { nixpkgs, utils, purs-nix, ... }:
    utils.defaultSystems
      ({ pkgs, system }:
         let
           pn = purs-nix { inherit system; };
           inherit (pn) purs ps-pkgs ps-pkgs-ns;

           inherit
             (purs
                { inherit (import ./package.nix pn) dependencies;
                  test-dependencies = [ ps-pkgs."assert" ];
                  src = ./src;
                }
             )
             shell;
         in
         { devShell =
             with pkgs;
             mkShell
               { buildInputs =
                   [ nodejs
                     nodePackages.bower
                     nodePackages.pulp
                     purescript
                     (shell {})
                   ];
               };
         }
      )
      nixpkgs;
}
