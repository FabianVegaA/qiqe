{ pkgs }: rec {
  packages = pkgs.poetry2nix.mkPoetryApplication {
    projectDir = ./.;
    overrides = [ pkgs.poetry2nix.defaultPoetryOverrides ];
  };

  shell = pkgs.mkShell {
    buildInputs = with pkgs; [ poetry ];
    inputsFrom = [ packages.dependencyEnv ];
  };

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
}
