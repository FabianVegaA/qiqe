{
  description = "Qiqe configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};

      in {
        packages = {
          default = pkgs.hello;
          auth-server = pkgs.poetry2nix.mkPoetryApplication {
            projectDir = ./site/server/auth;
            overrides = [ pkgs.poetry2nix.defaultPoetryOverrides ];
          };
        };

        devShell = pkgs.mkShell {
          buildInputs = let
            nixPackages = with pkgs; [ nixfmt ];
            haskellPackages = with pkgs.haskellPackages; [
              haskell-language-server
              ghcid
              cabal-install
            ];
            pythonPackages = with pkgs; [ poetry ];
            tsPackages = with pkgs; [ nodejs-16_x ];
            # elmPackages = with pkgs; [ nodePackages.elm nodePackages.elm-land ];
          in builtins.concatLists [
            nixPackages
            # haskellPackages
            pythonPackages
            # tsPackages
          ];
        };

        apps = {
          auth-server = {
            type = "app";
            program = let
              script = pkgs.writeShellApplication {
                name = "auth-server";
                runtimeInputs = [];
                text = ''
                  export PGSERVICEFILE="$PWD/data/.pg_service.conf"
                  export PGPASSFILE="$PWD/data/.pgpass"

                  cd site/server/auth
                  poetry run manage migrate
                  poetry run manage runserver --noreload
                '';
              };
            in "${script}/bin/auth-server";
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

            postgrest = {
              type = "app";
              program = let
                script = pkgs.writeShellApplication {
                  name = "postgrest";
                  runtimeInputs = [ pkgs.postgrest ];
                  text = ''
                    postgrest data/db.conf
                  '';
                };
              in "${script}/bin/postgrest";
            };
          };
        };
      });
}
