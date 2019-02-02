#! /usr/bin/env nix-shell
#! nix-shell deps.nix -i bash

set -e # exit with nonzero exit code if anything fails

(raco pkg catalog-show frog >> /dev/null) || raco pkg install frog markdown --auto

mkdir -p out

# Generate Blog

cp -f -r images out/.
cp -f -r js out/.

mkdir -p out/css
for file in scss/*.scss
do
  name=${file##*/}
  base=${name%.scss}
  sassc -t compressed -m inline $file out/css/$base.css
done

raco frog --clean -bp

