{ pkgs }: {
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
}
