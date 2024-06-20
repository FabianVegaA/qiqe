{ pkgs ? import <nixpkgs> { }, ghc }:
with pkgs;
haskell.lib.buildStackProject {
  inherit ghc;
  name = "Interpreter";
  buildInputs = [ zlib ];
}
