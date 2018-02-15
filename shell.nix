{ghc}:

with import <nixpkgs> {};

haskell.lib.buildStackProject {
  inherit ghc;
  name = "generic-servant";

  buildInputs = with pkgs; [
    zlib
  ];
}
