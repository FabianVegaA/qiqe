{
  description = "Qiqe configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ overlay ];
        };

        overlay = (final: prev: {
          qiqe = (final.callPackage ./. { } // {
            # Qiqe services
            auth-client = final.callPackage ./site/client/auth { };
            auth-server = final.callPackage ./site/server/auth { };
            interpreter = final.callPackage ./site/server/Interpreter { };

            # Development environment applications
            postgres = final.callPackage ./nix/postgres.nix { };
            postgrest = final.callPackage ./nix/postgrest.nix { };
            createdb = final.callPackage ./nix/createdb.nix { };

          });
        });

        devEnv = pkgs.mkShell { buildInputs = let nixPackages = with pkgs; [ nixfmt ]; in nixPackages; };
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
        };

        devShells = {
          default = devEnv;
          auth-client = pkgs.qiqe.auth-client.shell;
          auth-server = pkgs.qiqe.auth-server.shell;
          interpreter = pkgs.qiqe.interpreter.shell;
        };
        devShell = mergeEnvs pkgs (with devShells; [ auth-client auth-server ]);

        apps = {
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
          postgres = pkgs.qiqe.postgres;
          createdb = pkgs.qiqe.createdb;
          postgrest = pkgs.qiqe.postgrest;
        };
      });
}
