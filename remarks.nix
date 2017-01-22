{ mkDerivation, base, directory, filepath, GenericPretty, pretty
, stdenv, tasty, tasty-golden, tasty-hunit, containers
}:
mkDerivation {
  pname = "remarks";
  version = "0.1.10";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base GenericPretty pretty ];
  executableHaskellDepends = [
    base directory filepath GenericPretty
  ];
  testHaskellDepends = [
    base GenericPretty tasty tasty-golden tasty-hunit
  ];
  homepage = "https://github.com/DIKU-EDU/remarks#readme";
  description = "A DSL for marking student work";
  license = stdenv.lib.licenses.bsd3;
}
