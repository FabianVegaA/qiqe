{ pkgs }: {
  packages = pkgs.haskell.lib.buildStackProject {
    name = "interpreter";
    src = ./.;
  };
  shell = pkgs.mkShell {
    buildInputs = with pkgs.haskellPackages; [
      ghcid 
      ormolu
      hlint
      hoogle
      haskell-language-server
      implicit-hie
      retrie
    ];
  };
  script = pkgs.writeShellApplication {
    name = "interpreter";
    text = ''
      cd site/server/Interpreter 
      stack run
    '';
  };
}
