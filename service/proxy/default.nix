{ pkgs, system, env }: let 
  rustc_1_73_0 = (pkgs.callPackage (builtins.fetchGit {
      name = "pkgs-rustc-1.73.0";
      url = "https://github.com/NixOS/nixpkgs/";
      ref = "refs/heads/nixpkgs-unstable";
      rev = "459104f841356362bfb9ce1c788c1d42846b2454";
  }) { system = system; }).rustc;
in {
  packages = pkgs.rustBuilder.makePackageSet {
    rustVersion = "1.73.0";
    packageFun = import ./Cargo.nix;
  };
  
  shell = pkgs.mkShell {
    buildInputs = [
      pkgs.gcc
      pkgs.openssl
      pkgs.pkg-config
      pkgs.cargo
      pkgs.protobuf
      pkgs.rustfmt
      rustc_1_73_0
    ];
  };
  script = pkgs.writeShellApplication {
    name = "proxy";
    runtimeInputs = [
      pkgs.gcc
      pkgs.openssl
      pkgs.pkg-config
      pkgs.cargo
      pkgs.protobuf
      rustc_1_73_0
    ];
    text = ''
      cd service/proxy
      export CODEGEN_URI=${env.CODEGEN_URI}
      export CLIENT_URI=${env.CLIENT_URI}
      export OUT_DIR=$PWD/src

      # Wait for codegen service to be available
      while ! curl -s $CODEGEN_URI > /dev/null; do
        echo "Waiting for codegen service to be available in $CODEGEN_URI"
        sleep 1
      done
      # Execute
      if [[ ! -d "$OUT_DIR" ]]; then
          cargo run --bin build-proto
      fi
      cargo run --bin proxy
    '';
  };
}
