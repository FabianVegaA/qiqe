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
            auth-client = final.callPackage ./site/client/auth { };
            auth-server = final.callPackage ./site/server/auth { };
            interpreter = final.callPackage ./site/server/interpreter { };
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

          postgres = {
            type = "app";
            program = let
              script = pkgs.writeShellApplication {
                name = "pg_start";
                runtimeInputs = [ pkgs.postgresql ];
                text = ''
                  # Initialize a database with data stored in current project dir
                  [ ! -d "./data/db" ] && initdb --no-locale -D ./data/db

                  postgres -D ./data/db -k "$PWD"/data
                '';
              };
            in "${script}/bin/pg_start";
          };

          createdb = {
            type = "app";
            program = let
              script = pkgs.writeShellApplication {
                name = "createdb";
                runtimeInputs = [ pkgs.postgresql pkgs.openssl ];
                text = ''
                  # Create a database of your current user
                  if ! psql -h "$PWD"/data -lqt | cut -d \| -f 1 | grep -qw "$(whoami)"; then
                    createdb -h "$PWD"/data "$(whoami)"
                  fi

                  # Generate a password
                  PG_PASSWORD=$(openssl rand -base64 32)

                  # Load DB dump
                  cat <<EOF > db.sql
                  create role authenticator noinherit login password '$PG_PASSWORD';
                  create role qiqe_user nologin;
                  create database authenticator owner authenticator;
                  EOF

                  psql -h "$PWD"/data < db.sql

                  # Create .pgpass
                  echo "localhost.authenticator.$(whoami).$PG_PASSWORD" > "$PWD"/data/.pgpass && chmod 600 "$PWD"/data/.pgpass

                  # Create .pg_service.conf
                  cat <<EOF > "$PWD"/data/.pg_service.conf && chmod 600 "$PWD"/data/.pg_service.conf
                  [authenticator]
                  host=localhost
                  user=$(whoami)
                  dbname=authenticator
                  port=5432
                  EOF
                '';
              };
            in "${script}/bin/createdb";
          };

          postgrest = {
            type = "app";
            program = let
              script = pkgs.writeShellApplication {
                name = "pgREST";
                runtimeInputs = [ pkgs.postgrest ];
                text = ''
                  postgrest data/db.conf
                '';
              };
            in "${script}/bin/postgrest";
          };

        };
      });
}
