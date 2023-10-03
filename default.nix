{ writeShellScriptBin, qiqe }: {
  dev = writeShellScriptBin "dev" ''
    rm -rf ./node_modules
    ln -s ${qiqe.auth-client.nodeDependencies}/lib/node_modules ./node_modules
    export PATH="${qiqe.auth-client.nodeDependencies}/bin:$PATH"
    nix develop --command "cd auth-client && npm start"
  '';
}