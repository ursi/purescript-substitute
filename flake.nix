{
  outputs = { self, nixpkgs, utils }:
    utils.simpleShell
      [
        "dhall"
        "nodejs"
        "nodePackages.bower"
        "nodePackages.pulp"
        "purescript"
        "spago"
      ]
      nixpkgs;
}
