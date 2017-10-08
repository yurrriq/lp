with import <nixpkgs> {};


let

  # gap = callPackage ./nix/gap.nix {};

in


stdenv.mkDerivation rec {
  name = "howtogap-${version}";
  version = builtins.readFile ./VERSION;
  src = ./.;

  buildInputs = [
    gap

    # coreutils
    less
    # which
  ];

  buildFlags = [ "GAPROOT=${gap}/share/gap/build-dir" ];

  installPhase = ''
    ${gap}/bin/gap.sh -b makedoc.g
    local pkgdir=$out/share/gap/build-dir/pkg/aiig
    mkdir -p $pkgdir
    cp -R {PackageInfo,init,makedoc,read}.g docs/ gap/ tst/ $pkgdir
  '';
}
