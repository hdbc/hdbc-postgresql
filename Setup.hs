#!/usr/bin/env runhaskell
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

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
    lbi <- confHook simpleUserHooks pkg flags
    bi <- psqlBuildInfo lbi
    
    return lbi {
      localPkgDescr = updatePackageDescription
                        (Just bi, [("runtests", bi)]) (localPkgDescr lbi)
    } 
}

-- 'ConstOrId' is a @Cabal-1.16@ vs @Cabal-1.18@ compatibility hack,
-- 'programFindLocation' has a new (unused in this case)
-- parameter. 'ConstOrId' adds this parameter when types say it is
-- mandatory.
class ConstOrId a b where
    constOrId :: a -> b

instance ConstOrId a a where
    constOrId = id

instance ConstOrId a (b -> a) where
    constOrId = const

pgconfigProgram = (simpleProgram "pgconfig or pg_config") {
    programFindLocation = \verbosity -> constOrId $ do
      pgconfig  <- findProgramLocation verbosity "pgconfig"
      pg_config <- findProgramLocation verbosity "pg_config"
      return (pgconfig `mplus` pg_config)
  }

psqlBuildInfo :: LocalBuildInfo -> IO BuildInfo
psqlBuildInfo lbi = do
  (pgconfigProg, _) <- requireProgram verbosity
                         pgconfigProgram (withPrograms lbi)
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
