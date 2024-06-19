{ pkgs }: {
  script = pkgs.writeShellApplication {
    name = "gen-cargo";
    runtimeInputs = let
        # nix develop github:cargo2nix/cargo2nix#bootstrap
        cargo2nix = let
            src = pkgs.fetchFromGitHub {
              owner = "cargo2nix";
              repo = "cargo2nix";
              rev = "v0.11.0";
              sha256 = "b7ToXDqgTXsAWPluHEiFmiqaJwIrdSyJgyAOBfty5xo=";
            };
          in pkgs.callPackage "${src}/default.nix" { };
      in [ pkgs.cargo cargo2nix pkgs.tree ];
    text =  ''
      cd site/server/proxy
      
      cargo generate-lockfile
      cargo2nix
    '';
  };
}