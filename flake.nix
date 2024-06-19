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
            auth-client = final.callPackage ./site/client/auth { };
            auth-server = final.callPackage ./site/server/auth { };
            interpreter = final.callPackage ./site/server/Interpreter { };
            proxy = final.callPackage ./site/server/proxy { system = system; };

            # Build tools
            gen-cargo = final.callPackage ./nix/gen-cargo.nix { };
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
          auth-server = pkgs.qiqe.auth-server.packages;
          auth-client = pkgs.qiqe.auth-client.packages;
          interpreter = pkgs.qiqe.interpreter.packages;
          proxy = pkgs.qiqe.proxy.packages;
        };

        devShells = {
          default = devEnv;
          auth-client = pkgs.qiqe.auth-client.shell;
          auth-server = pkgs.qiqe.auth-server.shell;
          interpreter = pkgs.qiqe.interpreter.shell;
          proxy = pkgs.qiqe.proxy.shell;
        };
        devShell = mergeEnvs pkgs (with devShells; [ auth-client auth-server ]);

        apps = {
          proxy = {
            type = "app";
            program = "${pkgs.qiqe.proxy.script}/bin/proxy";
          };
          interpreter = {
            type = "app";
            program = "${pkgs.qiqe.interpreter.script}/bin/interpreter";
          };
          auth-server = {
            type = "app";
            program = "${pkgs.qiqe.auth-server.script}/bin/auth-server";
          };
          auth-client = {
            type = "app";
            program = "${pkgs.qiqe.auth-client.script}/bin/auth-client";
          };
          dev = {
            type = "app";
            program = let
              script = pkgs.writeShellApplication {
                name = "dev";
                runtimeInputs = [ pkgs.concurrently ];
                text = ''
                  echo "Starting development environment"
                  echo "Press Ctrl+C to stop\n"
                  npx concurrently                                             \
                    --names "proxy,auth-client,interpreter"                    \
                    --prefix-colors "bgMagenta.bold,bgBlue.bold,bgYellow.bold" \
                    --kill-others                                              \
                    --success first                                            \
                    --prefix "[{name}]"                                        \
                    --prefix-length 3                                          \
                    --timestamp-format "HH:mm:ss"                              \
                    --timestamp-prefix "[{time}]"                              \
                    --command                                                  \
                      "echo 'Interpreter'; ${apps.interpreter.program}"        \
                      "echo 'Proxy'; ${apps.proxy.program}"                    \
                      "echo 'Auth-Client'; ${apps.auth-client.program}"
                '';
              };
            in "${script}/bin/dev";
          };

          gen-cargo = {
            type = "app";
            program = "${pkgs.qiqe.gen-cargo.script}/bin/gen-cargo";
          };

          gen = {
            type = "app";
            program = let
              script = pkgs.writeShellApplication {
                name = "gen";
                runtimeInputs = [ pkgs.concurrently ];
                text = ''
                  npx concurrently                                             \
                    --names "gen-cargo"                                        \
                    --prefix-colors "bgMagenta.bold"                            \
                    --kill-others                                              \
                    --success first                                            \
                    --prefix "[{name}]"                                        \
                    --prefix-length 3                                          \
                    --timestamp-format "HH:mm:ss"                              \
                    --timestamp-prefix "[{time}]"                              \
                    --command                                                  \
                      "echo 'Gen-Cargo'; ${apps.gen-cargo.program}"
                '';
              };
            in "${script}/bin/gen";
          };
        };
      });
}
