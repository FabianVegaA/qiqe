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

        devEnv = pkgs.mkShell {
          buildInputs = let nixPackages = with pkgs; [ nixfmt ]; in nixPackages;
        };
        mergeEnvs = pkgs: envs:
          pkgs.mkShell (builtins.foldl' (a: v: {
            buildInputs = a.buildInputs ++ v.buildInputs;
            nativeBuildInputs = a.nativeBuildInputs ++ v.nativeBuildInputs;
            propagatedBuildInputs = a.propagatedBuildInputs
              ++ v.propagatedBuildInputs;
            propagatedNativeBuildInputs = a.propagatedNativeBuildInputs
              ++ v.propagatedNativeBuildInputs;
            shellHook = a.shellHook + "\n" + v.shellHook;
          }) (devEnv) envs);

      in rec {
        packages = {
          auth-server = pkgs.qiqe.auth-server.server;
          auth-client = pkgs.qiqe.auth-client.static;
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
            program = "${pkgs.qiqe.interpreter}/bin/interpreter";
          };

          auth-server = {
            type = "app";
            program = let
              script = pkgs.writeShellApplication {
                name = "auth-server";
                runtimeInputs = [ pkgs.openssl ];
                text = ''
                  export PGSERVICEFILE="$PWD/data/.pg_service.conf"
                  export PGPASSFILE="$PWD/data/.pgpass"

                  DJANGO_SUPERUSER_USERNAME="$(whoami)-$(openssl rand -base64 32 | tr -dc 'a-z0-9' | fold -w 8 | head -n 1)"
                  DJANGO_SUPERUSER_EMAIL="$DJANGO_SUPERUSER_USERNAME@admin.com" # TODO: use a real domain name
                  DJANGO_SUPERUSER_PASSWORD="$(openssl rand -base64 32)"

                  export DJANGO_SUPERUSER_USERNAME
                  export DJANGO_SUPERUSER_EMAIL
                  export DJANGO_SUPERUSER_PASSWORD

                  cd site/server/auth

                  cat <<EOF > .env
                  DJANGO_SUPERUSER_USERNAME=$DJANGO_SUPERUSER_USERNAME
                  DJANGO_SUPERUSER_EMAIL=$DJANGO_SUPERUSER_EMAIL
                  DJANGO_SUPERUSER_PASSWORD=$DJANGO_SUPERUSER_PASSWORD
                  EOF

                  poetry run manage migrate
                  poetry run manage createsuperuser --noinput
                  poetry run manage runserver --noreload
                '';
              };
            in "${script}/bin/auth-server";
          };

          auth-client = {
            type = "app";
            program = let
              script = pkgs.writeShellApplication {
                name = "auth-client";
                runtimeInputs = [ ];
                text = ''
                  cd site/client/auth
                  npm start
                '';
              };
            in "${script}/bin/auth-client";
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
