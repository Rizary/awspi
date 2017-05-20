{-# LANGUAGE CPP #-}

import Distribution.Simple

#if !defined(ghcjs_HOST_OS) && defined(MIN_VERSION_cabal_macosx)
import Distribution.MacOSX

guiApps :: [MacApp]
guiApps = [
    MacApp "awspi-frontend"
      Nothing
      (Just "macos/Info.plist")
      [
        "resources/static/stylesheet/simple.css"
      , "resources/index.html"
      , "resources/static/svg/ec2.svg"
      , "resources/static/svg/AmazonCloudFront.png"
      , "resources/static/svg/AmazonCloudWatch.png"
      , "resources/static/svg/AmazonDynamoDB.png"
      , "resources/static/svg/AmazonEC2.png"
      , "resources/static/svg/AmazonElasticCache.png"
      , "resources/static/svg/AmazonLightsail.png"
      , "resources/static/svg/AmazonRDS.png"
      , "resources/static/svg/AmazonRoute53.png"
      , "resources/static/svg/AmazonS3.png"
      , "resources/static/svg/AmazonVPC.png"
      ]
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