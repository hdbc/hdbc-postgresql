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
    programFindLocation = \verbosity -> constOrId $ do
      pgconfig  <- findProgramOnSearchPath verbosity [ProgramSearchPathDefault] "pgconfig"
      pg_config <- findProgramOnSearchPath verbosity [ProgramSearchPathDefault] "pg_config"
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
    extraLibDirs = [strip libDir],
    includeDirs  = [strip incDir]
  }
  where
    verbosity = normal -- honestly, this is a hack
    strip x = dropWhile isSpace $ reverse $ dropWhile isSpace $ reverse x
