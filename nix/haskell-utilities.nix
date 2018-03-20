{ nixpkgsFile ? <nixpkgs> 
, nixpkgsWith ? import nixpkgsFile
, nixpkgs     ? nixpkgsWith {}      # a.k.a. `import <nixpkgs> {}` 
}:

/*
sdistTarball       
triggerRebuild     
buildStackProject  
buildStrictly      
*/

########################################
let

### "IMPORTS"

inherit (nixpkgs) pkgs;
inherit (pkgs)    stdenv;
# "import" utilities
inherit (pkgs)       fetchFromGitHub;
inherit (stdenv.lib) optionalAttrs;

haskellLib =
 (import "${nixpkgs.path}/pkgs/development/haskell-modules/lib.nix")
  { pkgs = nixpkgs; };

haskell = pkgs.haskell.lib; 


########################################
let

/* : FilePath -> Derivation

runCabal2Nix ../enumerate
=
runCabal2NixWithString "" ../enumerate
~
cabal2nix file://"../enumerate" >"$out"

*/
runCabal2Nix = src:
  runCabal2NixWithString "" src

/* : {String: String} -> FilePath -> Derivation

runCabal2NixWithRecord { subpath: "enumerate-function"; } ../enumerate
=
runCabal2NixWithString ''--subpath "enumerate-function"'' ../enumerate
~
cabal2nix --subpath "enumerate-function" file://"../enumerate" >"$out"

*/
runCabal2NixWithRecord = options: 
  runCabal2NixWithString (record2CmdlnFlags options)

/* : String -> FilePath -> Derivation

runCabal2NixWithString ''--subpath "enumerate-function"'' ../enumerate
~
cabal2nix --subpath "enumerate-function" file://"../enumerate" >"$out"

*/
runCabal2NixWithString = options: src:
  nixpkgs.runCommand "cabal2nix" {

    buildCommand = ''
      cabal2nix ${options} file://"${src}" >"$out"
    '';

    buildInputs = with nixpkgs; [
      cabal2nix
    ];
  } "";

/*

*/
record2CmdlnFlags = kvs:
 attrValues
  (mapAttrs renderFlag
   kvs)

/*

> renderFlag "f" "orphans"
''-f"orphans"''

> renderFlag "flag" "orphans"
''--flag orphans''

*/
renderFlag = k: v:
 if   stringLength k >= 2
 then renderLongFlag k v

 else
 if   stringLength k == 1
 then renderShortFlag k v

 else
 abort (''empty flag in: [renderFlag "${k}" "${v}"]'')


/*

> renderShortFlag "f" "orphans"
''-f"orphans"''

*/
renderShortFlag = k: v:
 ''-${k}"${v}"''

/*

> renderLongFlag "flag" "orphans"
''--flag orphans''

*/
renderLongFlag = k: v:
 ''--${k} "${v}"''

in
########################################
let

### SOURCE OVERRIDES

# "megarepos" which have multiple packages as subdirectories.
repositories = {

};

# 
sources = {
};

in

########################################
### Haskell Dependencies...
let

/*

NOTES

* `local` / `github`: 
   They call `import` directly, thus those directories require a `default.nix`
* `cabal2nix` / `hackage` / `github2nix`: 
   They call `cabal2nix`, which generates the `default.nix`, so they don't require the given directory to be a valid `nix` package. 

TYPES
(in pseudo-typed-nix)

type Dependencies = { (Derivation | _) }

nix  : Path -> Dependencies -> Derivation
nix_ : Path ->              -> Derivation

hackage  : String/Name -> String/Version -> Dependencies -> Derivation
hackage_ : String/Name -> String/Version ->              -> Derivation                       

...


# `github`: 
             # o ::
             #      { owner           :: String
             #        repo            :: String
             #        rev             :: String
             #        fetchSubmodules :: Bool
             #        sha256          :: String
             #      } 

*/
utilitiesWith = pkgs: self: super: let

 # pseudo-import
 haskellLib = pkgs.haskell.lib;
 
 # aliases with names that make more sense to me
 haskell = haskellLib // (with haskellLib;
  { skipTests       = dontCheck
  ; jailbreak       = doJailbreak
  ; dropUpperBounds = doJailbreak
  ; dependsOn       = package: dependencies: 
                      addBuildDepends package dependencies;
  ;}
 
 # get a package via different locations and in different formats,
 # then build it directly via `callPackage` or indirectly via `cabal2nix`. 

 nixWith        = path:
                  self.callPackage path; 

 # localWith      = path:
 #                  self.callPackage path; 

 githubWith     = o:
                  self.callPackage (pkgs.fetchFromGitHub o); 

 cabal2nixWith  = name: source: 
                  self.callCabal2nix name source;

 hackageWith    = name: version:
                  self.callHackage name version;

 github2nixWith = o:
                  cabal2nix o.repo (pkgs.fetchFromGitHub o); 

 # override the package without any package-specific overriding of dependencies
 nix        = path:           nixWith        path         {};
 # local      = path:           localWith      path         {};
 github     = o:              githubWith     o            {};
 cabal2nix  = name: source:   cabal2nixWith  name source  {};
 hackage    = name: version:  hackageWith    name version {};
 github2nix = o:              github2nixWith o            {};

 in

 {
  inherit haskell;

  inherit dependsOn;

  inherit nix        nixWith;
  # inherit local  localWith;
  inherit github     githubWith;
  inherit cabal2nix  cabal2nixWith;
  inherit hackage    hackageWith;
  inherit github2nix github2nixWith;

 }

  # inherit nix     /* local */     github     cabal2nix     hackage     github2nix     ;
  # inherit nixWith /* localWith */ githubWith cabal2nixWith hackageWith github2nixWith ;

myHaskellOverlaysWith = pkgs: self: super: let
#myHaskellOverlaysWith = pkgs: self: super: let

 nix        = path:
              self.callPackage path; 

 local      = path:
              self.callPackage path; 

 github     = o:
              self.callPackage (pkgs.fetchFromGitHub o); 

             # o ::
             #      { owner           :: String
             #        repo            :: String
             #        rev             :: String
             #        fetchSubmodules :: Bool
             #        sha256          :: String
             #      } 

 cabal2nix  = name: source: 
              self.callCabal2nix name source;

 hackage    = name: version:
              self.callHackage name version;

 github2nix = o:
              cabal2nix o.repo (pkgs.fetchFromGitHub o); 

 # override the package without overriding any dependencies
 nix_        = path:           nix        path         {};
 local_      = path:           local      path         {};
 github_     = o:              github     o            {};
 cabal2nix_  = name: source:   cabal2nix  name source  {};
 hackage_    = name: version:  hackage    name version {};
 github2nix_ = o:              github2nix o            {};

 #
 #haskell = pkgs.haskell.lib; 
 dependsOn = package: dependencies: 
  haskell.addBuildDepends package dependencies;

 in

 # let 
 # reflex_dom = (import repositories.reflex-dom) self pkgs;
 # in

 {
   ########################################
   # Add Haskell Packages Below           #
   ######################################## 

  # protolude = hackage_ "protolude" "0.2.1";

  # vinyl = skipTests super.vinyl; 

#
# test-suite doctests
#   build-depends:    base >= 4.7 && <= 5, lens, vinyl, doctest >= 0.8, singletons >= 0.10
#
# Configuring singletons-2.3.1...
# Setup: Encountered missing dependencies:
# base >=4.10 && <5
# builder for ‘/nix/store/qiaqn6ni9c8c6w8drlkx9iyf1djyx6r7-singletons-2.3.1.drv’ failed with exit code 1
# building path(s) ‘/nix/store/gi2hv1j8pbb5cn55h9lgx11g4ajryys1-protolude-0.2.1-doc’, ‘/nix/store/rz0rwxdl4n5cva0gdbs24zw8zc2p4iqk-protolude-0.2.1’
# cannot build derivation ‘/nix/store/1a61yj01hgv3f35324p0gfj8gn1327jy-vinyl-0.7.0.drv’: 1 dependencies couldn't be built
# killing process 14726
# cannot build derivation ‘/nix/store/p1m3pi9y22gpmar7bs0wb0lpvkkf37k8-ghc-8.0.2-with-packages.drv’: 1

 };

in
########################################
let

### OTHER OVERRIDES
 
modifiedHaskellPackages = customizedHaskellPackages.override {
#  overrides = self: super: {
  overrides = self: super:
    myHaskellOverlaysWith pkgs self super // {
  };
};
in

########################################
let
### "CMDLN" OPTIONS 

inherit (stdenv.lib)
 id foldl' mapAttrs attrValues 
 ;

compose =
 f: g: x: f (g x)
 ;

# boolean eliminator
bool = x: y: b:
 if b then x else y
 ;

/* : { String : (Bool -> (Derivation -> Derivation)) }

mapping between my alias for a configuration option (short to be specified at the command line), and the toggling functions of that configuration option

*/
aliasedDerivationTransformers = self: 

  { test                = bool self.doCheck
                               self.dontCheck 
  
  ; bench               = bool self.doBenchmark
                               self.dontBenchmark 
  
  ; haddock             = bool (compose self.doHyperlinkSource self.doHaddock)
                               self.dontHaddock 
  
  ; coverage            = bool self.doCoverage
                               self.dontCoverage 
  
  ; static              = bool self.enableStaticLibraries
                               self.disableStaticLibraries 
    
  ; shared              = bool self.enableSharedLibraries
                               self.disableSharedLibraries 
    
  ; sharedExecutables   = bool self.enableSharedExecutables
                               self.disableSharedExecutables 
  
  ; strip               = bool self.doStrip
                               self.dontStrip 
  
  ; goldLinker          = bool self.linkWithGold
                               id 
  
  ; deadCodeElimination = bool self.enableDeadCodeElimination
                               self.disableDeadCodeElimination 
  
  ; dwarfDebugging      = bool self.enableDWARFDebugging
                               self.disableDWARFDebugging 

  ; strict              = bool id# self.buildStrictly
                               id 

  ; checkUnusedPackages = bool (self.checkUnusedPackages {})
                               id

  ;};

/*NOTES

http://gsc.io/nixos/nixpkgs/manual/

10.5.4.8.1. failOnAllWarnings
Applying haskell.lib.failOnAllWarnings to a Haskell package enables the -Wall and -Werror GHC options to turn all warnings into build failures.


10.5.4.8.2. buildStrictly
Applying haskell.lib.buildStrictly to a Haskell package calls failOnAllWarnings on the given package to turn all warnings into build failures. Additionally the source of your package is gotten from first invoking cabal sdist to ensure all needed files are listed in the Cabal file.


10.5.4.8.3. checkUnusedPackages
Applying haskell.lib.checkUnusedPackages to a Haskell package invokes the packunused tool on the package. packunused complains when it finds packages listed as build-depends in the Cabal file which are redundant. For example:

    $ nix-build -E 'let pkgs = import <nixpkgs> {}; in pkgs.haskell.lib.checkUnusedPackages {} pkgs.haskellPackages.scientific'
    these derivations will be built:
      /nix/store/3lc51cxj2j57y3zfpq5i69qbzjpvyci1-scientific-0.3.5.1.drv
    ...
    detected package components
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
     - library
     - testsuite(s): test-scientific
     - benchmark(s): bench-scientific*
    
    (component names suffixed with '*' are not configured to be built)
    
    library
    ~~~~~~~
    
    The following package dependencies seem redundant:
    
     - ghc-prim-0.5.0.0
    
    testsuite(test-scientific)
    ~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    no redundant packages dependencies found
    
    builder for ‘/nix/store/3lc51cxj2j57y3zfpq5i69qbzjpvyci1-scientific-0.3.5.1.drv’ failed with exit code 1
    error: build of ‘/nix/store/3lc51cxj2j57y3zfpq5i69qbzjpvyci1-scientific-0.3.5.1.drv’ failed

As you can see, packunused finds out that although the testsuite component has no redundant dependencies the library component of scientific-0.3.5.1 depends on ghc-prim which is unused in the library.
    
*/


/* : String -> Bool -> (Derivation -> Derivation)

*/
translateMyOptionToANixHaskellDerivationTransformer
 = self: optionName: optionValue:
   (aliasedDerivationTransformers self).${optionName} optionValue #TODO `self`
 ;

/* : List (Derivation -> Derivation)

*/
theseOptions = self:
 attrValues
  (mapAttrs (translateMyOptionToANixHaskellDerivationTransformer self) 
   options)
 ;

/* : Derivation -> Derivation

NOTE 

foldl :: (b ->        a -> b) -> b ->      [a] -> b

($) :: (a -> b) -> a -> b
($) = id @(a -> b)

(.) :: (a -> a) -> (a -> a) -> (a -> a)

specializations:

foldl :: (b ->        a -> b) -> b ->      [a] -> b
foldl :: (b -> (b -> b) -> b) -> b -> [b -> b] -> b

foldl :: (b        ->        a ->        b) ->       b ->       [a] -> b
foldl :: ((a -> a) -> (a -> a) -> (a -> a)) -> (a -> a) -> [a -> a] -> (a -> a)

foldl :: ((Derivation -> Derivation) -> (Derivation -> Derivation) -> (Derivation -> Derivation)) -> (Derivation -> Derivation) -> [Derivation -> Derivation] -> (Derivation -> Derivation)

*/
thisOverride = self:
 foldl' compose id (theseOptions self)
 ;

/* NOTES

==========


==========

fold...

  # Strict version of `foldl'.
     The difference is that evaluation is forced upon access. Usually used
     with small whole results (in contract with lazily-generated list or large
     lists where only a part is consumed.)
  #
  foldl' = builtins.foldl' or foldl;

  # “left fold”, like `foldr', but from the left:
     `foldl op nul [x_1 x_2 ... x_n] == op (... (op (op nul x_1) x_2) ... x_n)`.
     Type:
       foldl :: (b -> a -> b) -> b -> [a] -> b
     Example:
       lconcat = foldl (a: b: a + b) "z"
       lconcat [ "a" "b" "c" ]
       => "zabc"
       # different types
       lstrange = foldl (str: int: str + toString (int + 1)) ""
       strange [ 1 2 3 4 ]
       => "a2345"
  #
  foldl = op: nul: list:
    let
      len = length list;
      foldl' = n:
        if n == -1
        then nul
        else op (foldl' (n - 1)) (elemAt list n);
    in foldl' (length list - 1);

==========

attrValues...

  # Return the values of all attributes in the given set, sorted by
     attribute name.
     Example:
       attrValues {c = 3; a = 1; b = 2;}
       => [1 2 3]
  #
  attrValues = builtins.attrValues or (attrs: attrVals (attrNames attrs) attrs);


==========

mapAttrs...

  # Apply a function to each element in an attribute set.  The
     function takes two arguments --- the attribute name and its value
     --- and returns the new value for the attribute.  The result is a
     new attribute set.
     Example:
       mapAttrs (name: value: name + "-" + value)
          { x = "foo"; y = "bar"; }
       => { x = "x-foo"; y = "y-bar"; }
  #
  mapAttrs = f: set:
    listToAttrs (map (attr: { name = attr; value = f attr set.${attr}; }) (attrNames set));


==========

*/

in
########################################
