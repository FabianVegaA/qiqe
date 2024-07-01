{ pkgs, env }: {
  script = pkgs.writeShellApplication {
    name = "ngrok";
    runtimeInputs = [ pkgs.ngrok ];
    text = ''
      ngrok config add-authtoken ${env.NGROK_AUTH_TOKEN}
      ngrok http http://localhost:3030
      ''
  }
}