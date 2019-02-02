# All Nix dependencies for developing DokMelody

with import <nixpkgs> {};

let

  # Show lentil output using Emacs compatible info, and managing also md files.
  wrappedLentil = stdenv.mkDerivation {
    name = "lentil-all";

    buildInputs = [ haskellPackages.lentil which ];

    unpackPhase = "true";
    # NOTE: skip the phase executing the shell command "true"

    installPhase = ''
      mkdir -p "$out/bin"
      echo "#! ${stdenv.shell}" >> "$out/bin/lentil-all"
      echo "exec $(which lentil) . -a md%txt -f comp" >> "$out/bin/lentil-all"
      chmod u+x "$out/bin/lentil-all"
    '';
  };

in runCommand "dummy" {
     buildInputs = [
       python
       pythonPackages.pygments
       racket
       sassc
       haskellPackages.lentil
       wrappedLentil
       wget
    ];
} ""
