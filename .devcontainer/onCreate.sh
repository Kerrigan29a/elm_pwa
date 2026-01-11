#!/usr/bin/env bash
set -euo pipefail

# Install Elm tooling once when the container is created to improve cacheability
# Pin versions to Elm 0.19.1 toolchain to avoid drift.
# REMEMBER: Update .github/workflows/static.yml when updating this version.
npm install -g \
  elm@0.19.1 \
  elm-format
  elm-test
