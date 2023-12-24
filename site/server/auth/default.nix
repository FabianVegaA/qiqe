{ pkgs }: let 
  poetry2nix = import (pkgs.fetchFromGitHub {
    owner = "nix-community";
    repo = "poetry2nix";
    rev = "master";
    sha256 = "sha256-p6niqag7b4XEHvzWgG0X/xjoW/ZXbAxW8ggd8yReT3Y=";
  }) { };
in rec {
  packages = poetry2nix.mkPoetryApplication {
    projectDir = ./.;
    overrides = [ poetry2nix.defaultPoetryOverrides ];
  };

  shell = pkgs.mkShell {
    buildInputs = with pkgs; [ poetry ];
    inputsFrom = [ packages.dependencyEnv ];
    shellHook = ''
      echo "LD_LIBRARY_PATH: $LD_LIBRARY_PATH"
    '';
    LD_LIBRARY_PATH="${pkgs.stdenv.cc.cc.lib}/lib";
  };

  script = pkgs.writeShellApplication {
    name = "auth-server";
    runtimeInputs = [ pkgs.openssl ];
    text = ''
      # Verify that LD_LIBRARY_PATH is set correctly
      export LD_LIBRARY_PATH="${pkgs.stdenv.cc.cc.lib}/lib"

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

  container = pkgs.dockerTools.buildImage {
    name = "auth-server";
    tag = "latest";
    copyToRoot = pkgs.buildEnv {
      name = "auth-server";
      paths = [ packages script ];
      pathsToLink = [ "/bin" ];
    };
    config = {
      Cmd = [ "/bin/manage" ];
      WorkingDir = "/data";
      ExposedPorts = {
        "8000/tcp" = {};
      };
      Env = [
        "PGSERVICEFILE=/data/.pg_service.conf"
        "PGPASSFILE=/data/.pgpass"
      ];
      Volumes = {
        "/data" = {};
      };
    };
  };
}
