#!/usr/bin/env runhaskell

import Distribution.Simple
import Distribution.PackageDescription
import Distribution.Version

import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Verbosity

import Data.Char (isSpace)
import Data.List (dropWhile,reverse)

import Control.Monad

main = defaultMainWithHooks simpleUserHooks {
  hookedPrograms = [pgconfigProgram],

  confHook = \pkg flags -> do
    lbi <- confHook defaultUserHooks pkg flags
    bi <- psqlBuildInfo lbi
    return lbi {
      localPkgDescr = updatePackageDescription
                        (Just bi, [(f"runtests", bi)]) (localPkgDescr lbi)
    } 
}

pgconfigProgram = (simpleProgram "pgconfig") {
    programFindLocation = \verbosity -> do
      pgconfig  <- findProgramOnPath "pgconfig"  verbosity 
      pg_config <- findProgramOnPath "pg_config" verbosity
      return (pgconfig `mplus` pg_config)
  }

psqlBuildInfo :: LocalBuildInfo -> IO BuildInfo
psqlBuildInfo lbi = do
  (pgconfigProg, _) <- requireProgram verbosity
                         pgconfigProgram AnyVersion (withPrograms lbi)
  let pgconfig = rawSystemProgramStdout verbosity pgconfigProg

  incDir <- pgconfig ["--includedir"]
  libDir <- pgconfig ["--libdir"]

  return emptyBuildInfo {
    extraLibDirs = [strip libDir],
    includeDirs  = [strip incDir]
  }
  where
    verbosity = normal -- honestly, this is a hack
    strip x = dropWhile isSpace $ reverse $ dropWhile isSpace $ reverse x
