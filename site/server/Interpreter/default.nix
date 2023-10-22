{ pkgs }: {
  packages = pkgs.haskell.lib.buildStackProject {
    name = "interpreter";
    src = ./.;
  };
  shell = pkgs.mkShell { buildInputs = [ ]; };
  script = pkgs.writeShellApplication {
    name = "interpreter";
    text = ''
      cd site/server/Interpreter 
      stack run
    '';
  };
}
