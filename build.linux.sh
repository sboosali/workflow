#!/bin/bash
set -e
########################################

time  stack build  --stack-yaml stack-linux.yaml  -j6  "$@" 

########################################

#NOTES
#  --stack-yaml STACK-YAML  Override project stack.yaml file (overrides any
#                           STACK_YAML environment variable)
