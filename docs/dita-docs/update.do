#!/usr/bin/env rc

# TODO force execution when the content of the directory change, or generate always

cd dita-docs

DST_DIR='../../_build/docs'
CURR_DIR=`{pwd}

mkdir -p $DST_DIR
dita --input=website.ditamap --format=html5-bootstrap --output=^$DST_DIR --args.input.dir=^$CURR_DIR --propertyfile=website.properties

mkdir -p $DST_DIR^/images
rsync -rutp images/ $DST_DIR^/images

# This is an hack for generating a poor-man sitemap
# and a poor-man index.html redirecting to a valid position
cp -f resources/index.html $DST_DIR^/.

cd $DST_DIR
mv toc.html sitemap.html
