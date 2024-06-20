{ pkgs, system }: {
  packages = pkgs.rustBuilder.makePackageSet {
    rustVersion = "1.73.0";
    packageFun = import ./Cargo.nix;
  };
  shell = pkgs.mkShell {
    buildInputs = let
      pkgs = import (builtins.fetchGit {
         name = "pkgs-rustc-1.73.0";
         url = "https://github.com/NixOS/nixpkgs/";
         ref = "refs/heads/nixpkgs-unstable";
         rev = "459104f841356362bfb9ce1c788c1d42846b2454";
     }) { system = system; };
     rustc_1_73_0 = pkgs.rustc;
    in [ pkgs.cargo pkgs.protobuf rustc_1_73_0 ];
  };
  script = pkgs.writeShellApplication {
    name = "proxy";
    text = ''
      cd service/proxy
      export CODEGEN_URI="http://0.0.0.0:50051"
      export OUT_DIR=$PWD/src
      if [[ ! -d "$OUT_DIR" ]]; then
        cargo run --bin build-proto
      fi
      cargo run --bin proxy
    '';
  };
}
