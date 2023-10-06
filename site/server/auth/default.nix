{ pkgs }: rec {
  server = pkgs.poetry2nix.mkPoetryApplication {
    projectDir = ./.;
    overrides = [ pkgs.poetry2nix.defaultPoetryOverrides ];
  };

  shell = with pkgs; [ poetry ];
}
