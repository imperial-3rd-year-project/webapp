#!/bin/bash

echo "Setting up Haskell dependencies"
stack install yesod-bin --install-ghc
git submodule update --init --recursive
echo "Building project"
stack build
echo "Installing dependencies"
npm install
echo "Running..."
electron .
