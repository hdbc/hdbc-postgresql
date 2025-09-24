#!/usr/bin/env runhaskell
{-# LANGUAGE CPP, MultiParamTypeClasses, FlexibleInstances #-}

import Distribution.Simple
import Distribution.PackageDescription
import Distribution.Version

import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Verbosity
#if MIN_VERSION_Cabal(3,14,0)
import Distribution.Utils.Path
#endif

import Data.Char (isSpace)
import Data.List (dropWhile,reverse)
import Data.String (fromString)

import Control.Monad

main = defaultMainWithHooks simpleUserHooks {
  hookedPrograms = [pgconfigProgram],

  confHook = \pkg flags -> do
    lbi <- confHook simpleUserHooks pkg flags
    bi <- psqlBuildInfo lbi
    
    return lbi {
      localPkgDescr = updatePackageDescription
                        (Just bi, [(fromString "runtests", bi)]) (localPkgDescr lbi)
    } 
}

-- 'ConstOrId' is a @Cabal-1.16@ vs @Cabal-1.18@ compatibility hack,
-- 'programFindLocation' has a new (unused in this case)
-- parameter. 'ConstOrId' adds this parameter when types say it is
-- mandatory.
class FindProgramLocation a b where
    constOrId :: a -> b

instance FindProgramLocation (IO (Maybe FilePath)) (IO (Maybe FilePath)) where
    constOrId = id

instance FindProgramLocation (IO (Maybe FilePath)) (ProgramSearchPath -> IO (Maybe FilePath)) where
    constOrId = const

instance FindProgramLocation (IO (Maybe FilePath)) (ProgramSearchPath -> IO (Maybe (FilePath, [FilePath]))) where
    constOrId x = liftM (fmap (\x -> (x, []))) . const x

pgconfigProgram = (simpleProgram "pgconfig or pg_config") {
    programFindLocation = \verbosity searchPath -> do
      pgconfig  <- findProgramOnSearchPath verbosity searchPath "pgconfig"
      pg_config <- findProgramOnSearchPath verbosity searchPath "pg_config"
      return (pgconfig `mplus` pg_config)
  }

psqlBuildInfo :: LocalBuildInfo -> IO BuildInfo
psqlBuildInfo lbi = do
  (pgconfigProg, _) <- requireProgram verbosity
                         pgconfigProgram (withPrograms lbi)
  let pgconfig = getProgramOutput verbosity pgconfigProg

  incDir <- pgconfig ["--includedir"]
  libDir <- pgconfig ["--libdir"]

  return emptyBuildInfo {
    extraLibDirs = [toPath $ strip libDir],
    includeDirs  = [toPath $ strip incDir]
  }
  where
#if MIN_VERSION_Cabal(3,14,0)
    toPath = makeSymbolicPath
#else
    toPath = id
#endif
    verbosity = normal -- honestly, this is a hack
    strip x = dropWhile isSpace $ reverse $ dropWhile isSpace $ reverse x
