{ nixpkgsFile ? <nixpkgs> 
, nixpkgsWith ? import nixpkgsFile
, nixpkgs     ? nixpkgsWith {}             # a.k.a. `import <nixpkgs> {}` 

, pkgs            ? nixpkgs.pkgs
, haskellPackages ? pkgs.haskellPackages

, utilities ? (import ./utilities.nix) { inherit nixpkgs pkgs; }

}:

/* of the arguments to this file, only `nixpkgs` and `haskellPackages` are referenced below; 
the rest are just intermediary expressions / convenience "functions". 

*/

########################################
# Imports
let

inherit (pkgs)       stdenv;

inherit (pkgs)       fetchFromGitHub;
inherit (stdenv.lib) optionalAttrs;

haskell = pkgs.haskell.lib; 

# with utilities;
# assertEnum 

in
########################################
# Helpers
let

call = src:
 call' src {};

call' = src: ps: 
  haskellPackages.callPackage src ps;

in
########################################

{

workflow-x11-shell = call ./workflow-x11-shell;

workflow-types     = call ./workflow-types;
workflow-extra     = call ./workflow-extra;
workflow-pure      = call ./workflow-pure;

}

########################################
