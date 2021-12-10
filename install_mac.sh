#!/bin/bash

brew update
brew install ghc
brew install pyenv
pyenv install 3.10.0
pyenv local 3.10.0
python -m venv .env
pip install -r requirements.txt