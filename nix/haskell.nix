{ nixpkgsFile ? <nixpkgs> 
/* : FilePath

the "raw" `nixpkgs` repository. 

Overriden by `nixpkgs` or `nixpkgsWith` (may be ignored if they are provided explicitly; in particular they're ignored if the argument parameter expression doesn't reference `nixpkgsWith` and/or `nixpkgsFile`).
*/

, nixpkgsWith ? import nixpkgsFile
/* : PackageSetConfig -> PackageSet
where
type PackageSet = {<PackageName>: <Derivation>, ...}

a configurable `nixpkgs` expression.

Overriden by `nixpkgs` (may be ignored if it is provided, in particular it's ignored if the argument parameter expression doesn't reference `nixpkgsWith` and/or `nixpkgsFile`). 
*/

, nixpkgs ? nixpkgsWith {}
/* : PackageSet 

i.e. 
```
nixpkgs ? import <nixpkgs> {}
```

you can configure the default (or a custom one) with custom overlays or whatever, e.g.:

nixpkgs = nixpkgsWith { overlays = [(self: super: ...), ...] }

*/

, pkgs ? nixpkgs.pkgs
/* : PackageSet 

the top-level package namespace. 

i.e. 
```
(import <nixpkgs> {}).pkgs
```

*/

# , haskellPackages ? pkgs.haskellPackages
# /* : PackageSet 
# 
# the default haskell package namespace. 
# i.e. 
# ```
# (import <nixpkgs> {}).pkgs.haskellPackages
# ```
# */

, packageDotNix ? null
/* : Maybe FilePath

An explicit/manual `<package>.nix` file. 

`null` means: use `cabal2nix ./.` (this behavior is controlled by `haskellPackages.callPackage`, indirectly)

=
null
./.
./default.nix
<etc>

the `.nix` must be in the same directory as the `.cabal`; in particular, not a symlink in the current directory that links to a file in another directory. 

$ find ./nix

*/

, compiler ? null
/* : Maybe String 

The haskell compiler version. 

`null` means: use the default, 
i.e. `haskellPackages` not `haskell.packages.${compiler}`

"ghc7103"
"ghc802"
"ghc822"
"ghc841"
"ghcHEAD"
"ghc7103Binary"
"ghc821Binary"
"ghcjs"
"ghcjsHEAD"
"integer-simple"

*/

, resolver ? null
/* : Maybe String

Whether to use the Stackage `PackageSet` and with which `lts` resolver (each resolver implicitly includes a specific compiler too).

* `null` means: don't use Stackage 
* `"default"` means: use Stackage, and with the latest (supported) one; currently `lts-107`
* `"lts-107"` means: LTS 10.7`, using ghc-8.2.2, and published on 2018-02-24.
* etc

`lts-<X.Y>` means: use this `Stackage` resolver, via `nixpkgs-stackage` (which is assumed to be in the `nixpkgs` namespace, i.e. have been added as an overlay)

https://www.stackage.org/lts-10.7

=
lts-107
lts-106
lts-105
lts-104
lts-103
lts-102
lts-101
lts-100
lts-921
...
lts-00 

https://github.com/typeable/nixpkgs-stackage

*/

, integer-simple ? false
/* : Boolean

`true` means: use the `integer-simple` compilers, not the `integer-gmp` ones (which are implicitly the default).

i.e. `haskell.packages.integer-simple.${compiler}` not `haskell.packages.${compiler}`

=
false
true
*/

/* : Boolean

the `isXxx` options mean: apply this option to the package itself only (c.f. `withXxx`).

*/

, withHoogle  ? false 
/* : Boolean
*/

#TODO, withLLVM    ? false
/* : Boolean
*/

, withProfiled    ? false
, withTested      ? false
, withBenchmarked ? false
, withDocumented  ? false
, withHyperlinked ? true
, withDwarf       ? false
/* : Boolean

the `withXxx` options mean: apply this option to all dependencies.

*/

, whichObjectLibraries ? "default"
/* : String

= 
"static" 
"shared"
"both"
*/

, whichLinker ? "default"
/* : String

= 
"gold"
TODO? 
*/

, options ? {}

/* options for building this package, by overriding the derivation. 

`{ <option> : null }` and `{}` are the same, 
i.e. fallback to the underlying defaults. 

`{ <option> : true }` 
is `do<Option>` or `enable<Option>` (or something else).

`{ <option> : false }` 
is `dont<Option>` or `disable<Option>` (or something else).

unknown options are currently silently ignored. 

: { test :: Maybe Bool
  # true  = doCheck
  # false = dontCheck
  # null  = id
  #
  
  , bench :: Maybe Bool
  # true  = doBenchmark
  # false = dontBenchmark
  # null  = id
  #
  
  , haddock :: Maybe Bool
  # true  = doHaddock
  # false = dontHaddock
  # null  = id
  #
  
  , coverage :: Maybe Bool
  # true  = doCoverage
  # false = dontCoverage
  # null  = id
  #
  
  , static :: Maybe Bool
  # true  = enableStaticLibraries
  # false = disableStaticLibraries
  # null  = id
  #
    
  , shared :: Maybe Bool
  # true  = enableSharedLibraries
  # false = disableSharedLibraries
  # null  = id
  #
    
  , sharedExecutables :: Maybe Bool
  # true  = enableSharedExecutables
  # false = disableSharedExecutables
  # null  = id
  #
  
  , strip :: Maybe Bool
  # true  = doStrip
  # false = dontStrip
  # null  = id
  #
  
  , goldLinker :: Maybe Bool
  # true  = linkWithGold
  # false = id?
  # null  = id
  #
  
  , deadCodeElimination :: Maybe Bool
  # true  = enableDeadCodeElimination
  # false = disableDeadCodeElimination
  # null  = id
  #
  
  , dwarfDebugging :: Maybe Bool
  # true  = enableDWARFDebugging
  # false = disableDWARFDebugging
  # null  = id
  #

  , checkUnusedPackages :: Maybe Bool
  # true  = checkUnusedPackages
  # false = id?
  # null  = id
  #

  , strict :: Maybe Bool
  # true  = buildStrictly
  # false = id?
  # null  = id
  #

  }

TODO
sdistTarball       
triggerRebuild     
buildStackProject  
buildStrictly      

*/
}:

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

/* 

> attrNames pkgs.haskell.lib
addBuildDepend             :: ? -> ?
addBuildDepends            :: ? -> ?
addBuildTool               :: ? -> ?
addBuildTools              :: ? -> ?
addExtraLibraries          :: ? -> ?
addExtraLibrary            :: ? -> ?
addPkgconfigDepend         :: ? -> ?
addPkgconfigDepends        :: ? -> ?
addSetupDepend             :: ? -> ?
addSetupDepends            :: ? -> ?
appendConfigureFlag        :: ? -> ?
appendPatch                :: ? -> ?
appendPatches              :: ? -> ?
buildFromSdist             :: ? -> ?
buildStackProject          :: ? -> ?
buildStrictly              :: ? -> ?
checkUnusedPackages        :: ? -> ?
controlPhases              :: ? -> ?
disableCabalFlag           :: ? -> ?
disableDeadCodeElimination :: ? -> ?
disableHardening           :: ? -> ?
disableLibraryProfiling    :: ? -> ?
disableSharedExecutables   :: ? -> ?
disableSharedLibraries     :: ? -> ?
disableStaticLibraries     :: ? -> ?
doBenchmark                :: ? -> ?
doCheck                    :: ? -> ?
doCoverage                 :: ? -> ?
doDistribute               :: ? -> ?
doHaddock                  :: ? -> ?
doHyperlinkSource          :: ? -> ?
doJailbreak                :: ? -> ?
doStrip                    :: ? -> ?
dontBenchmark              :: ? -> ?
dontCheck                  :: ? -> ?
dontCoverage               :: ? -> ?
dontDistribute             :: ? -> ?
dontHaddock                :: ? -> ?
dontHyperlinkSource        :: ? -> ?
dontJailbreak              :: ? -> ?
dontStrip                  :: ? -> ?
enableCabalFlag            :: ? -> ?
enableDWARFDebugging       :: ? -> ?
enableDeadCodeElimination  :: ? -> ?
enableLibraryProfiling     :: ? -> ?
enableSharedExecutables    :: ? -> ?
enableSharedLibraries      :: ? -> ?
enableStaticLibraries      :: ? -> ?
extractBuildInputs         :: ? -> ?
failOnAllWarnings          :: ? -> ?
getHaskellBuildInputs      :: ? -> ?
ghcInfo                    :: ? -> ?
justStaticExecutables      :: ? -> ?
linkWithGold               :: ? -> ?
makePackageSet             :: ? -> ?
markBroken                 :: ? -> ?
markBrokenVersion          :: ? -> ?
overrideCabal              :: ? -> ?
overrideSrc                :: ? -> ?
packageSourceOverrides     :: ? -> ?
removeConfigureFlag        :: ? -> ?
sdistTarball               :: ? -> ?
shellAware                 :: ? -> ?
triggerRebuild             :: ? -> ?

configuration transformers:

doCheck                    :: ? -> ?

doBenchmark

doHaddock                  :: ? -> ?
doHyperlinkSource          :: ? -> ?
doCoverage                 :: ? -> ?

doJailbreak                :: ? -> ?

linkWithGold

enableDWARFDebugging       :: ? -> ?

enableDeadCodeElimination  :: ? -> ?

enableLibraryProfiling     :: ? -> ?

enableSharedExecutables    :: ? -> ?

enableSharedLibraries      :: ? -> ?
enableStaticLibraries      :: ? -> ?

*/

in
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
let

inherit (stdenv.lib)
 isString
 ;

# TODO nix-shell --show-trace -p "(haskell.packages.${COMPILER}.override { overrides = self: super: { spiros = haskell.lib.dontCheck (haskell.lib.dontHaddock (self.callCabal2nix ''{{name}}'' ./. {})); }; }).ghcWithPackages (self: with self; [ {{name}} ])"

####################

customMkDerivation = self: super: args:
  super.mkDerivation
    (args // customDerivationOptions);

customDerivationOptions = 
    { enableLibraryProfiling = withProfiled; 
      doCheck                = withTested; 
      doBenchmark            = withBenchmarked; 
      doHaddock              = withDocumented;
      doHyperlinkSource      = withDocumented && withHyperlinked;
      enableDWARFDebugging   = withDwarf;
    } //
    ( if   (whichObjectLibraries == "shared") 
      then { enableSharedLibraries  = true; 
           }
      else 
      if   (whichObjectLibraries == "static")
      then { enableStaticLibraries  = true; 
           }
      else
      if   (whichObjectLibraries == "both") # TODO
      then { enableSharedLibraries  = true;
             enableStaticLibraries  = true; 
           }
      else 
      if   (whichObjectLibraries == "default")
      then {}
      else {} # TODO
    ) // 
    optionalAttrs (whichLinker == "gold") 
      { linkWithGold = true;
      }
 ;

####################

hooglePackagesOverride = self: super:
  {
    ghcWithPackages = self.ghc.withPackages;

    ghc = super.ghc //
      { withPackages = super.ghc.withHoogle; 
      };
  };

####################

####################

# llvmPackagesOverride = self: super:
#   {
#     ghcWithPackages = self.ghc.withPackages;

#     ghc = super.ghc //
#       { withPackages = super.ghc.llvmPackages; #TODO
#       };
#   };

### COMPILERS

haskellPackagesWithCompiler1 = 
  if   (compiler == null) || (compiler == "default")
       #TODO `integer-simple` is ignored if this matches
  then pkgs.haskellPackages

  else 
  if   integer-simple
  then pkgs.haskell.packages.integer-simple.${compiler}

  else 
  if   isString resolver
  then pkgs.haskell.packages.stackage.${resolver} # e.g. "lts-107"

  else pkgs.haskell.packages.${compiler};

haskellPackagesWithCustomPackages2 =
  if   withHoogle
  then haskellPackagesWithCompiler1.override {
         overrides = hooglePackagesOverride;
       }
  else haskellPackagesWithCompiler1;

haskellPackagesWithCustomMkDerivation3 = 
  haskellPackagesWithCustomPackages2.override {
    overrides = self: super: {
      mkDerivation = customMkDerivation self super;
    };
  };

# the last referenced, below
# TODO scoping
customizedHaskellPackages = haskellPackagesWithCustomMkDerivation3;

/*
nix-repl> haskell.packages._

haskell.packages.ghc7103
haskell.packages.ghc821Binary
haskell.packages.ghcHEAD
haskell.packages.integer-simple
haskell.packages.ghc7103Binary
haskell.packages.ghc822
haskell.packages.ghcjs
haskell.packages.ghc802
haskell.packages.ghc841
haskell.packages.ghcjsHEAD       

*/

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
