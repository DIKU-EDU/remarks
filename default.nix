# This default.nix produces a statically linked executable with no
# library dependencies.
let pkgs = import <nixpkgs> {};
in pkgs.haskell.lib.overrideCabal
  (pkgs.haskell.packages.ghc910.callCabal2nix "remarks" ./. {})
  ( _drv: {
    isLibrary = false;
    isExecutable = true;
    enableSharedExecutables = false;
    enableSharedLibraries = false;
    configureFlags = [
      "--ghc-option=-split-sections"
      "--ghc-option=-optl=-static"
      "--ghc-option=-optl=-lbz2"
      "--ghc-option=-optl=-lz"
      "--ghc-option=-optl=-lelf"
      "--ghc-option=-optl=-llzma"
      "--ghc-option=-optl=-lzstd"
      "--extra-lib-dirs=${pkgs.glibc.static}/lib"
      "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
      "--extra-lib-dirs=${pkgs.zlib.static}/lib"
      "--extra-lib-dirs=${(pkgs.xz.override { enableStatic = true; }).out}/lib"
      "--extra-lib-dirs=${(pkgs.zstd.override { enableStatic = true; }).out}/lib"
      "--extra-lib-dirs=${(pkgs.bzip2.override { enableStatic = true; }).out}/lib"
      "--extra-lib-dirs=${(pkgs.elfutils.overrideAttrs (old: { dontDisableStatic = true; })).out}/lib"
      "--extra-lib-dirs=${pkgs.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
    ];
  }
  )
