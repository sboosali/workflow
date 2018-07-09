#!/bin/bash
set -e

stack --nix --stack-yaml=stack/spiros.yaml build
