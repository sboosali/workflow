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

# the target is both the 'component' and the sub-directory. 
TARGET=$(basename "${1:-all}")
# must strip any trailing slashes, 
# which lets us tab-complete via the eponymous subdirs in a terminal. 

GHCID_FILE="./ghcid.txt"

########################################
echo

echo '[TARGET]'
echo "$TARGET" 
echo 

echo '[GHCID_FILE]'
echo "$GHCID_FILE" 
echo 

########################################

echo '...' > "$GHCID_FILE" 
# "$EDITOR" ./ghcid.txt &

# ghcid  --command "cabal new-repl $TARGET"  --project="$TARGET/"  --reload="$TARGET"  --restart="$TARGET/*.cabal"  --outputfile "$GHCID_FILE"

ghcid  --directory="$TARGET/"  --command "cabal new-repl $TARGET"  --reload="./sources/"  --project="$TARGET"  --reload="$TARGET"  --restart="./*.cabal"  --outputfile "$GHCID_FILE"

########################################

# $ ghcid --help
# 
#  -c --command=COMMAND  Command to run (defaults to ghci or cabal repl)
# 
#  -T --test=EXPR        Command to run after successful loading
# 
#  -W --warnings         Allow tests to run even with warnings
# 
#  -n --no-title         Don't update the shell title/icon
# 
#  -p --project=NAME     Name of the project, defaults to current directory
# 
#     --reload=PATH      Reload when the given file or directory contents
#                        change (defaults to none)
# 
#     --restart=PATH     Restart the command when the given file or directory
#                        contents change (defaults to .ghci and any .cabal file)
# 
#  -C --directory=DIR    Set the current directory
# 
#  -o --outputfile=FILE  File to write the full output to
# 
#
#

# GHCID_FILE="$TARGET/ghcid.txt"
# ghcid  --command "cabal new-repl $TARGET"  --project="$TARGET/"  --reload="$TARGET"  --restart="$TARGET/*.cabal"  --outputfile "$GHCID_FILE" 
# ^   /home/sboo/haskell/workflow/sources/Workflow/: can't watch what isn't there!: does not exist

