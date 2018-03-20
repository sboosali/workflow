{ nixpkgsFile ? <nixpkgs> 
, nixpkgsWith ? import nixpkgsFile
, nixpkgs     ? nixpkgsWith {}             # a.k.a. `import <nixpkgs> {}` 

, pkgs             ? nixpkgs.pkgs
, haskellUtilities ? pkgs.haskell.lib 
, haskellPackages  ? pkgs.haskellPackages

}@arguments:

########################################
# (Documentation)

/* Parameters

##########

nixpkgsFile
: FilePath

the "raw" `nixpkgs` repository. 

Overriden by `nixpkgs` or `nixpkgsWith` (may be ignored if they are provided explicitly; in particular they're ignored if the argument parameter expression doesn't reference `nixpkgsWith` and/or `nixpkgsFile`).

##########

nixpkgsWith 
: PackageSetConfig -> PackageSet

where
type PackageSet = {<PackageName>: <Derivation>, ...}

a configurable `nixpkgs` expression.

Overriden by `nixpkgs` (may be ignored if it is provided, in particular it's ignored if the argument parameter expression doesn't reference `nixpkgsWith` and/or `nixpkgsFile`).

##########

nixpkgs
: PackageSet 

i.e. 
nixpkgs ? import <nixpkgs> {}

you can configure the default (or a custom one) with custom overlays or whatever, e.g.:

nixpkgs = nixpkgsWith { overlays = [(self: super: ...), ...] }

##########


##########


##########

Of the above arguments to this file, only `nixpkgs` and `haskellPackages` are referenced below (including transitively; despite them passed-through to the other `haskell-<...>.nix` modules); the rest are just intermediary expressions / convenience "functions". 

*/
########################################
# Imports
let

inherit (pkgs)    stdenv;
# "import" utilities
inherit (stdenv)       mkDerivation;
# inherit (pkgs)       fetchFromGitHub;
inherit (stdenv.lib)
 filter elem any concatMap
 ;

projectUtilities = import ./project.nix arguments; 

inherit (projectUtilities)
 getBuildDepends
 asBuildDepends
 getBuildInputs
 asBuildInputs
 ;

in
########################################
# Helpers
let

noAdditionalDependencies = f: src:
 f src {};

directory2string = path:
 toString (baseNameOf path); 

project2dependencies = ps:
 let 
 isProjectDependency = p:
  !(p == null) && !(isProjectPackage p);
 isProjectPackage = p:
  elem p ps || any (isNamesake p) ps;
 isNamesake = p: q:
  p.pname == q.pname;
  # (p.pname == q.pname) || (p.name == q.name);
 allDependencies =
  concatMap package2dependencies ps;
 in
 filter isProjectDependency allDependencies
 ;

package2dependencies = p:
 getBuildInputs p
 # getBuildInputs p
 # getBuildDepends p
 # p.propagatedBuildInputs ++ p.propagatedNativeBuildInputs
 # haskellUtilities.getHaskellBuildInputs p
 # p.env
 ;
in
########################################
let

sboosaliHaskellPackages = haskellPackages.override {
  overrides = sboosaliOverrides;
};

sboosaliOverrides = self: super: 
  let
  call = noAdditionalDependencies callCabal2nix;
  callCabal2nix = src:
    let name = directory2string src; in
    super.callCabal2nix name src; 
  callPackage = src: ps: 
    self.callPackage src ps;
  in
  {
    spiros    = call ../spiros;
    enumerate = call ../enumerate/enumerate;
  }
;

in
########################################
let

workflowHaskellPackages = sboosaliHaskellPackages.override {
  overrides = workflowOverrides;
};

workflowOverrides = self: super: 
  let
  call = noAdditionalDependencies callCabal2nix;
  callCabal2nix = src:
    let name = directory2string src; in
    super.callCabal2nix name src; 
  callPackage = src: ps: 
    self.callPackage src ps;
  in
  #TODO project = {}
  {
    workflow-types     = call ./workflow-types;
    workflow-extra     = call ./workflow-extra;
    workflow-pure      = call ./workflow-pure;

    workflow-x11-shell = call ./workflow-x11-shell;  
    workflow-osx       = call ./workflow-osx;  

    # workflow-windows  = call ./workflow-windows;
  }
;

#TODO
workflow = self: 
  {
    types     = self.workflow-types;
    extra     = self.workflow-extra;
    pure      = self.workflow-pure;
    linux     = self.workflow-x11-shell;  
    osx       = self.workflow-osx;  
  };

workflowDependencies =
 let 
 project = workflowHaskellPackages;

 linux = linuxOnly ++ common;

 osx = osxOnly ++ common;

 common = [
   project.workflow-types
   project.workflow-extra
 ];

 linuxOnly = [
   project.workflow-x11-shell
 ];

 osxOnly = [
   project.workflow-osx
 ];

 # windows = [
 #   project.workflow-windows
 # ];
 # miscellaneous = [
 #   project.workflow-pure
 # ];

 in
 {
  inherit linux osx;
 };

# linuxEnvironment = mkDerivation {
#   src         = ./.;
#   name        = "workflow-project-linux-environment";
#   version     = "0";
#   buildInputs = 
#     concatMap getBuildInputs workflowDependencies.linux;
#     # project2dependencies workflowDependencies.linux;
# };

linuxEnvironment = mkDerivation {
  src         = ./.;
  name        = "workflow-project-linux-environment";
  buildInputs = projectBuildInputsForLinux; 
};

projectBuildInputsForLinux =
 getBuildInputs workflowHaskellPackages.workflow-types 
 # getBuildInputs workflowHaskellPackages.workflow-types
 # concatMap getBuildInputs workflowDependencies.linux
 # project2dependencies workflowDependencies.linux;
;

workflowEnvironments = {
 linux = linuxEnvironment;
};

in
########################################
let

#environment = haskellUtilities.shellAware workflowEnvironments.linux;
environment = workflowEnvironments.linux;

in
########################################
environment


/*NOTES


  # Extract the haskell build inputs of a haskell package.
  # This is useful to build environments for developing on that
  # package.
  getHaskellBuildInputs = p:
    (p.override { mkDerivation = extractBuildInputs p.compiler;
                }).haskellBuildInputs;


  # Under normal evaluation, simply return the original package. Under
  # nix-shell evaluation, return a nix-shell optimized environment.
  shellAware = p: if lib.inNixShell then p.env else p;


>>> haskellPackages.spiros.buildInputs 
[ null «derivation /nix/store/ngf86qcxqg87fdak7z4ifqihwcgzmfxd-doctest-0.13.0.drv» «derivation /nix/store/2wz1mx4288d6j09kvb2warjibabp4vp8-tasty-0.11.3.drv» «derivation /nix/store/bp2b3dyhvv74p6m3p0mbwpqrinl0wa1m-tasty-hunit-0.9.2.drv» ]

>>> haskellPackages.spiros.propagatedBuildInputs 
[ null null null «derivation /nix/store/cvznil8zpf1cj27gpl4qnl56pqdfmrcl-data-default-class-0.1.2.0.drv» null null «derivation /nix/store/8sy3rymb9iq7xw8vgkxg96h32pxppnbb-exceptions-0.8.3.drv» «derivation /nix/store/xmv56p01j0dj4p6p8kc0h63w8qs08f4q-generic-deriving-1.12.1.drv» «derivation /nix/store/qiz037k7r16gzjkq94pi6qk6318zdalf-hashable-1.2.6.1.drv» «derivation /nix/store/xydlsbqxrbvdf8c0kjgr0k3bci0knmbr-mtl-2.2.1.drv» «derivation /nix/store/p00rxc4knqr2lzczl4zsk61339hkxj1m-prettyprinter-1.1.1.drv» null «derivation /nix/store/fla67x0az88xanfwsnyck34xdm8a74gv-safe-0.3.15.drv» «derivation /nix/store/7pxlfg14qpls07l6xdv0jiw8ad1vh995-safe-exceptions-0.1.6.0.drv» «derivation /nix/store/i1kwb2p1vjwkkj3zhr6nn7zppw9li8ji-split-0.2.3.3.drv» «derivation /nix/store/87m15j6ic1fplbr8gawwhn9fg52d2hxr-stm-2.4.5.0.drv» «derivation /nix/store/6lws43s4f45zfw0mdfgvi7vjz3q56sfi-string-conv-0.1.2.drv» null «derivation /nix/store/dl4r80mc02800fj17b1kx9lzxl9lfk8j-text-1.2.2.2.drv» null null «derivation /nix/store/kvq86mkp19v41q1rmqr5mbwi6006rc79-unordered-containers-0.2.8.0.drv» «derivation /nix/store/nkakscbdxzqwvxhp8nabj18j8288h8y9-vector-0.12.0.1.drv» «derivation /nix/store/1j9c5pablrd1w7wpabc3s9bs91lmizia-vinyl-0.7.0.drv» ]

>>> haskellPackages.spiros.propagatedNativeBuildInputs 
[ ]

>>> haskellUtilities.getHaskellBuildInputs haskellPackages.spiros
[ «derivation /nix/store/cvznil8zpf1cj27gpl4qnl56pqdfmrcl-data-default-class-0.1.2.0.drv» «derivation /nix/store/8sy3rymb9iq7xw8vgkxg96h32pxppnbb-exceptions-0.8.3.drv» «derivation /nix/store/xmv56p01j0dj4p6p8kc0h63w8qs08f4q-generic-deriving-1.12.1.drv» «derivation /nix/store/qiz037k7r16gzjkq94pi6qk6318zdalf-hashable-1.2.6.1.drv» «derivation /nix/store/xydlsbqxrbvdf8c0kjgr0k3bci0knmbr-mtl-2.2.1.drv» «derivation /nix/store/p00rxc4knqr2lzczl4zsk61339hkxj1m-prettyprinter-1.1.1.drv» «derivation /nix/store/fla67x0az88xanfwsnyck34xdm8a74gv-safe-0.3.15.drv» «derivation /nix/store/7pxlfg14qpls07l6xdv0jiw8ad1vh995-safe-exceptions-0.1.6.0.drv» «derivation /nix/store/i1kwb2p1vjwkkj3zhr6nn7zppw9li8ji-split-0.2.3.3.drv» «derivation /nix/store/87m15j6ic1fplbr8gawwhn9fg52d2hxr-stm-2.4.5.0.drv» «derivation /nix/store/6lws43s4f45zfw0mdfgvi7vjz3q56sfi-string-conv-0.1.2.drv» «derivation /nix/store/dl4r80mc02800fj17b1kx9lzxl9lfk8j-text-1.2.2.2.drv» «derivation /nix/store/kvq86mkp19v41q1rmqr5mbwi6006rc79-unordered-containers-0.2.8.0.drv» «derivation /nix/store/nkakscbdxzqwvxhp8nabj18j8288h8y9-vector-0.12.0.1.drv» «derivation /nix/store/1j9c5pablrd1w7wpabc3s9bs91lmizia-vinyl-0.7.0.drv» «derivation /nix/store/ngf86qcxqg87fdak7z4ifqihwcgzmfxd-doctest-0.13.0.drv» «derivation /nix/store/2wz1mx4288d6j09kvb2warjibabp4vp8-tasty-0.11.3.drv» «derivation /nix/store/bp2b3dyhvv74p6m3p0mbwpqrinl0wa1m-tasty-hunit-0.9.2.drv» ]


>>> haskellPackages.spiros.outputs
[ "out" "doc" ]

>>> haskellPackages.spiros.all
[ «derivation /nix/store/zk9129h6s39j0kwkqhfb5j5d1vc3dvwz-spiros-0.2.drv» «derivation /nix/store/zk9129h6s39j0kwkqhfb5j5d1vc3dvwz-spiros-0.2.drv» ]

>>> haskellPackages.spiros.env.name
"interactive-spiros-0.2-environment"

>>> haskellPackages.spiros.name
"spiros-0.2"

*/