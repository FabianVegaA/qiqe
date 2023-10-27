{ pkgs }: {
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

        rm db.sql

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

        exit 0
      '';
      
    };
  in "${script}/bin/createdb";
}
