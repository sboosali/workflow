#!/bin/bash
set -e

TARGET="${1:-all}"

cabal --project-file=cabal/spiros.project new-build "$TARGET"