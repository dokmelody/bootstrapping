#!/usr/bin/env rc

redo update-websites

RDIR='/srv/www/bootstrapping.dokmelody.org'
RHOST='dokmelody.org'

rsync --chown=nginx:nginx --delete -avzhe ssh _build/ root@^$RHOST:^$RDIR

RDIR2='/srv/www/www.dokmelody.org'
rsync --chown=nginx:nginx --delete -avzhe ssh www_dokmelody_org/ root@^$RHOST:^$RDIR2
