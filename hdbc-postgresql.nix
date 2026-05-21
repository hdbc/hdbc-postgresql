{ mkDerivation, base, bytestring, Cabal, convertible, HDBC, lib
, mtl, old-time, parsec, postgresql, time, utf8-string
}:
mkDerivation {
  pname = "HDBC-postgresql";
  version = "2.5.0.2";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  setupHaskellDepends = [ base Cabal ];
  libraryHaskellDepends = [
    base bytestring convertible HDBC mtl old-time parsec time
    utf8-string
  ];
  librarySystemDepends = [ postgresql (postgresql.pg_config) ];
  homepage = "http://github.com/hdbc/hdbc-postgresql";
  description = "PostgreSQL driver for HDBC";
  license = lib.licenses.bsd3;
}
