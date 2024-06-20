{
  description = "Qiqe configuration";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    cargo2nix.url = "github:cargo2nix/cargo2nix/release-0.11.0";
    nixpkgs.url = "github:nixos/nixpkgs";
    nixpkgs.follows = "cargo2nix/nixpkgs";
  };

  outputs = { self, nixpkgs, flake-utils, cargo2nix, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ cargo2nix.overlays.default overlay ];
        };

        overlay = (final: prev: {
          qiqe = (final.callPackage ./. { } // {
            # Qiqe services
            client = final.callPackage ./service/auth { };
            codegen = final.callPackage ./service/Interpreter { };
            proxy = final.callPackage ./service/proxy { system = system; };
          });
        });

        devEnv = pkgs.mkShell {
          buildInputs = let nixPackages = with pkgs; [ nixfmt node2nix ]; in nixPackages;
        };
        mergeEnvs = pkgs: envs:
          pkgs.mkShell (builtins.foldl' (a: v: {
            buildInputs = a.buildInputs ++ v.buildInputs;
            nativeBuildInputs = a.nativeBuildInputs ++ v.nativeBuildInputs;
            propagatedBuildInputs = a.propagatedBuildInputs ++ v.propagatedBuildInputs;
            propagatedNativeBuildInputs = a.propagatedNativeBuildInputs ++ v.propagatedNativeBuildInputs;
            shellHook = a.shellHook + "\n" + v.shellHook;
          }) (devEnv) envs);

      in rec {
        packages = {
          client = pkgs.qiqe.client.packages;
          codegen = pkgs.qiqe.codegen.packages;
          proxy = pkgs.qiqe.proxy.packages;
        };

        devShells = {
          default = devEnv;
          client = pkgs.qiqe.client.shell;
          codegen = pkgs.qiqe.codegen.shell;
          proxy = pkgs.qiqe.proxy.shell;
        };
        devShell = mergeEnvs pkgs (with devShells; [ client codegen proxy ]);

        apps = {
          proxy = {
            type = "app";
            program = "${pkgs.qiqe.proxy.script}/bin/proxy";
          };
          codegen = {
            type = "app";
            program = "${pkgs.qiqe.codegen.script}/bin/interpreter";
          };
          client = {
            type = "app";
            program = "${pkgs.qiqe.client.script}/bin/client";
          };
          dev = {
            type = "app";
            program = let
              script = pkgs.writeShellApplication {
                name = "dev";
                runtimeInputs = [ pkgs.concurrently ];
                text = ''
                  printf "Starting development environment"
                  printf "Press Ctrl+C to stop\n"
                  npx concurrently                                             \
                    --names "codegen,client,proxy"                             \
                    --prefix-colors "bgMagenta.bold,bgBlue.bold,bgYellow.bold" \
                    --kill-others                                              \
                    --success first                                            \
                    --prefix "[{name}]"                                        \
                    --prefix-length 3                                          \
                    --timestamp-format "HH:mm:ss"                              \
                    --timestamp-prefix "[{time}]"                              \
                    --command                                                  \
                      "echo 'Codegen'; ${apps.codegen.program}&"               \
                      "echo 'Client'; ${apps.client.program}&"                 \
                      "echo 'Proxy'; ${apps.proxy.program}&"
                '';
              };
            in "${script}/bin/dev";
          };
        };
      });
}
