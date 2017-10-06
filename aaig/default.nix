with import <nixpkgs> {};


let

  gap = callPackage ./nix/gap.nix {};

in


stdenv.mkDerivation rec {
  name = "howtogap-${version}";
  version = builtins.readFile ./VERSION;

  buildInputs = [
    gap
    less
  ];
}
