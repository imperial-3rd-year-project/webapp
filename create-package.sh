#!/bin/sh

rm -r bin/
mkdir bin
cp .stack-work/dist/x86_64-linux-tinfo6/Cabal-3.0.1.0/build/web-app/web-app ./bin/
electron-packager --overwrite .
cp -r ./bin/ ./webapp-linux-x64/
cp -r ./static/ ./webapp-linux-x64/
cp -r ./config/ ./webapp-linux-x64/

