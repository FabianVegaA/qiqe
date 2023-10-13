{ writeShellScriptBin, qiqe }: {
  dev = writeShellScriptBin "dev" ''
    rm -rf ./node_modules
    ln -s ${qiqe.auth-client.nodeDependencies}/lib/node_modules ./node_modules
    export PATH="${qiqe.auth-client.nodeDependencies}/bin:$PATH"
    nix develop --command npx concurrently \
      -n "auth-client,auth-server" \
      -c green,yellow \
      "cd auth-client && npm start" \
      "cd auth-server && poetry run manage runserver"
      ""
  '';
}
