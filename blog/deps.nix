# All dependencies for generating the website using Racket and Frog.

with import <nixpkgs> {};

runCommand "dummy" {
  buildInputs = [
    python
    pythonPackages.pygments
    racket
    tetex
    bibtex2html
    sassc
  ];
} ""
