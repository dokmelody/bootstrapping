#! /usr/bin/env nix-shell
#! nix-shell deps.nix -i bash

set -e # exit with nonzero exit code if anything fails

(raco pkg show frog >> /dev/null) || raco pkg install frog markdown --auto

mkdir -p out

# Generate Blog

cp -f -r images out/.
cp -f -r js out/.

mkdir -p out/css
for file in scss/*.scss
do
  name=${file##*/}
  base=${name%.scss}
  sassc -t compressed $file out/css/$base.css
done

# NOTE: these HTML documents must be updated manually inside Emacs org-mode, with "m e e h h" sequence
# and check no error messages are displayed, otherwise it will be used the last version of the document.
for d in dok-lang dokmelody scratchpad
do
    echo "Copying $d"
    mkdir -p out/$d
    cp -f -r ../docs/$d/* out/$d/.
done

raco frog --clean -bp

