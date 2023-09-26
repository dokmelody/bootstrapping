#!/usr/bin/env rc

redo update-websites

{sleep 3; xdg-open http://localhost:8080/}&

cd _build
python -m http.server 8080
