#!/usr/bin/env bash
set -euo pipefail

# Install Elm tooling once when the container is created to improve cacheability
npm install -g elm elm-format elm-test
