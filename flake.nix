{
  description = "Qiqe configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in {
        packages.hello = pkgs.hello;

        devShell = pkgs.mkShell {
          buildInputs = let 
            nodePackages = pkgs.nodePackages;
            haskellShell = with pkgs.haskellPackages; [
              haskell-language-server
              ghcid
              cabal-install
            ];
            pythonShell = with pkgs; [ poetry ];
            tsShell = with pkgs; [ nodejs-16_x ];
            # elmShell = with pkgs; [ nodePackages.elm nodePackages.elm-land ];
          in builtins.concatLists [ haskellShell pythonShell tsShell ];
        };
      });
}
