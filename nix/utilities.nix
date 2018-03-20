{ nixpkgsFile ? <nixpkgs> 
, nixpkgsWith ? import nixpkgsFile
, nixpkgs     ? nixpkgsWith {}             # a.k.a. `import <nixpkgs> {}` 

, pkgs ? nixpkgs.pkgs

}:

########################################
# Imports
let

#inherit (pkgs)       stdenv;

in
########################################
# Helpers
let

in
########################################

{

/* : {String: a} -> (String -> a)

*/
match = fs: k:
 fs.${k}
 ;

/* : [a] -> a -> Assertion

*/
assertEnum = xs: x:
 assert (builtins.elem x xs)
 ;

}

########################################
