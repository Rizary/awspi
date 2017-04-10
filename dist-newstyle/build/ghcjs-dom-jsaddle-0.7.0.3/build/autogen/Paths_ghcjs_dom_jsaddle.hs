{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_ghcjs_dom_jsaddle (
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
version = Version [0,7,0,3] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/Rizitecs/.cabal/bin"
libdir     = "/Users/Rizitecs/.cabal/lib/x86_64-osx-ghc-8.0.2/.fake.ghcjs-dom-jsaddle-0.7.0.3"
dynlibdir  = "/Users/Rizitecs/.cabal/lib/x86_64-osx-ghc-8.0.2"
datadir    = "/Users/Rizitecs/.cabal/share/x86_64-osx-ghc-8.0.2/ghcjs-dom-jsaddle-0.7.0.3"
libexecdir = "/Users/Rizitecs/.cabal/libexec"
sysconfdir = "/Users/Rizitecs/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ghcjs_dom_jsaddle_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ghcjs_dom_jsaddle_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "ghcjs_dom_jsaddle_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "ghcjs_dom_jsaddle_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ghcjs_dom_jsaddle_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ghcjs_dom_jsaddle_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
