#!/bin/bash
set -e
########################################

function get-platform-specific-project() {
 local OS=$(uname)
 local FILE=

 case "$OS" in
  Linux|linux) 
    FILE=cabal.linux.project 
    #TODO currently `workflow` just supports X11
    ;;

  Darwin|darwin)
    FILE=cabal.osx.project
    ;;

  *) 
    ;;

 esac

 echo "$FILE"
}

########################################

#NOTE the order of the action versus the target wrt this bash script is the same as the `cabal` command

# `cabal new-build all` by default

ACTION="${1:-build}"

TARGET="${2:-all}"

PROJECT_FILE=$(get-platform-specific-project)

########################################
echo

echo '[TARGET]'
echo "$TARGET" 
echo 

echo '[--project-file PROJECT_FILE]'
echo "$PROJECT_FILE"
echo 

########################################
cabal "new-$ACTION" "$TARGET" --project-file "$PROJECT_FILE"
########################################
