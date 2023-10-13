{ pkgs }: rec {
  overlay = final: prev: {
    interpreter = final.callCabal2nix "interpreter" ./. { };
  };
  packages = pkgs.haskellPackages.extend overlay;
  shell = packages.shellFor {
    packages = p: with p; [ interpreter ];
    buildInputs = with packages; [
      ghcid
      cabal-install
      haskell-language-server
    ];
  };
}
