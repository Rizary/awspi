{-# LANGUAGE CPP #-}

import Distribution.Simple

#if !defined(ghcjs_HOST_OS) && defined(MIN_VERSION_cabal_macosx)
import Distribution.MacOSX

guiApps :: [MacApp]
guiApps = [
    MacApp "awspi-frontend"
      Nothing
      (Just "macos/Info.plist")
      []
      []
      DoNotChase
   ]
#endif

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks
#if !defined(ghcjs_HOST_OS) && defined(MIN_VERSION_cabal_macosx)
       {
          postBuild = appBundleBuildHook guiApps,
          postCopy = appBundleCopyHook guiApps
       }
#endif