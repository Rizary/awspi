{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_vector_instances (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [3,4] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/Rizitecs/.cabal/bin"
libdir     = "/Users/Rizitecs/.cabal/lib/x86_64-osx-ghc-8.0.2/.fake.vector-instances-3.4"
dynlibdir  = "/Users/Rizitecs/.cabal/lib/x86_64-osx-ghc-8.0.2"
datadir    = "/Users/Rizitecs/.cabal/share/x86_64-osx-ghc-8.0.2/vector-instances-3.4"
libexecdir = "/Users/Rizitecs/.cabal/libexec"
sysconfdir = "/Users/Rizitecs/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "vector_instances_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "vector_instances_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "vector_instances_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "vector_instances_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "vector_instances_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "vector_instances_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
