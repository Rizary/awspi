{-# LANGUAGE PatternSynonyms #-}
-- For HasCallStack compatibility
{-# LANGUAGE ImplicitParams, ConstraintKinds, KindSignatures #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module JSDOM.Generated.SVGTests
       (hasExtension, hasExtension_, getRequiredFeatures,
        getRequiredFeaturesUnsafe, getRequiredFeaturesUnchecked,
        getRequiredExtensions, getRequiredExtensionsUnsafe,
        getRequiredExtensionsUnchecked, getSystemLanguage,
        getSystemLanguageUnsafe, getSystemLanguageUnchecked, SVGTests(..),
        gTypeSVGTests)
       where
import Prelude ((.), (==), (>>=), return, IO, Int, Float, Double, Bool(..), Maybe, maybe, fromIntegral, round, realToFrac, fmap, Show, Read, Eq, Ord, Maybe(..))
import qualified Prelude (error)
import Data.Typeable (Typeable)
import Language.Javascript.JSaddle (JSM(..), JSVal(..), JSString, strictEqual, toJSVal, valToStr, valToNumber, valToBool, js, jss, jsf, jsg, function, new, array)
import Data.Int (Int64)
import Data.Word (Word, Word64)
import JSDOM.Types
import Control.Applicative ((<$>))
import Control.Monad (void)
import Control.Lens.Operators ((^.))
import JSDOM.EventTargetClosures (EventName, unsafeEventName)
import JSDOM.Enums

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGTests.hasExtension Mozilla SVGTests.hasExtension documentation> 
hasExtension ::
             (MonadDOM m, ToJSString extension) =>
               SVGTests -> extension -> m Bool
hasExtension self extension
  = liftDOM
      ((self ^. jsf "hasExtension" [toJSVal extension]) >>= valToBool)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGTests.hasExtension Mozilla SVGTests.hasExtension documentation> 
hasExtension_ ::
              (MonadDOM m, ToJSString extension) => SVGTests -> extension -> m ()
hasExtension_ self extension
  = liftDOM (void (self ^. jsf "hasExtension" [toJSVal extension]))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGTests.requiredFeatures Mozilla SVGTests.requiredFeatures documentation> 
getRequiredFeatures ::
                    (MonadDOM m) => SVGTests -> m (Maybe SVGStringList)
getRequiredFeatures self
  = liftDOM ((self ^. js "requiredFeatures") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGTests.requiredFeatures Mozilla SVGTests.requiredFeatures documentation> 
getRequiredFeaturesUnsafe ::
                          (MonadDOM m, HasCallStack) => SVGTests -> m SVGStringList
getRequiredFeaturesUnsafe self
  = liftDOM
      (((self ^. js "requiredFeatures") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGTests.requiredFeatures Mozilla SVGTests.requiredFeatures documentation> 
getRequiredFeaturesUnchecked ::
                             (MonadDOM m) => SVGTests -> m SVGStringList
getRequiredFeaturesUnchecked self
  = liftDOM ((self ^. js "requiredFeatures") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGTests.requiredExtensions Mozilla SVGTests.requiredExtensions documentation> 
getRequiredExtensions ::
                      (MonadDOM m) => SVGTests -> m (Maybe SVGStringList)
getRequiredExtensions self
  = liftDOM ((self ^. js "requiredExtensions") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGTests.requiredExtensions Mozilla SVGTests.requiredExtensions documentation> 
getRequiredExtensionsUnsafe ::
                            (MonadDOM m, HasCallStack) => SVGTests -> m SVGStringList
getRequiredExtensionsUnsafe self
  = liftDOM
      (((self ^. js "requiredExtensions") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGTests.requiredExtensions Mozilla SVGTests.requiredExtensions documentation> 
getRequiredExtensionsUnchecked ::
                               (MonadDOM m) => SVGTests -> m SVGStringList
getRequiredExtensionsUnchecked self
  = liftDOM
      ((self ^. js "requiredExtensions") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGTests.systemLanguage Mozilla SVGTests.systemLanguage documentation> 
getSystemLanguage ::
                  (MonadDOM m) => SVGTests -> m (Maybe SVGStringList)
getSystemLanguage self
  = liftDOM ((self ^. js "systemLanguage") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGTests.systemLanguage Mozilla SVGTests.systemLanguage documentation> 
getSystemLanguageUnsafe ::
                        (MonadDOM m, HasCallStack) => SVGTests -> m SVGStringList
getSystemLanguageUnsafe self
  = liftDOM
      (((self ^. js "systemLanguage") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGTests.systemLanguage Mozilla SVGTests.systemLanguage documentation> 
getSystemLanguageUnchecked ::
                           (MonadDOM m) => SVGTests -> m SVGStringList
getSystemLanguageUnchecked self
  = liftDOM ((self ^. js "systemLanguage") >>= fromJSValUnchecked)
