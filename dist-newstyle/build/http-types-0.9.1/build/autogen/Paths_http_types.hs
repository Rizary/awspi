{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_http_types (
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
version = Version [0,9,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/Rizitecs/.cabal/bin"
libdir     = "/Users/Rizitecs/.cabal/lib/x86_64-osx-ghc-8.0.2/.fake.http-types-0.9.1"
dynlibdir  = "/Users/Rizitecs/.cabal/lib/x86_64-osx-ghc-8.0.2"
datadir    = "/Users/Rizitecs/.cabal/share/x86_64-osx-ghc-8.0.2/http-types-0.9.1"
libexecdir = "/Users/Rizitecs/.cabal/libexec"
sysconfdir = "/Users/Rizitecs/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "http_types_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "http_types_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "http_types_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "http_types_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "http_types_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "http_types_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
