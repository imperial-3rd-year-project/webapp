#!/bin/bash

echo "Setting up Haskell dependencies"
stack install yesod-bin --install-ghc
echo "Building project"
stack build
echo "Installing dependencies"
npm install
echo "Running..."
electron .
