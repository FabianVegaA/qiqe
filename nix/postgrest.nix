{ pkgs }: {
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
}
