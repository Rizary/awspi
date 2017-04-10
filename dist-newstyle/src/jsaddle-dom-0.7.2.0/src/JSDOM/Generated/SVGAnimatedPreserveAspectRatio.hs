{-# LANGUAGE PatternSynonyms #-}
-- For HasCallStack compatibility
{-# LANGUAGE ImplicitParams, ConstraintKinds, KindSignatures #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module JSDOM.Generated.SVGAnimatedPreserveAspectRatio
       (getBaseVal, getBaseValUnsafe, getBaseValUnchecked, getAnimVal,
        getAnimValUnsafe, getAnimValUnchecked,
        SVGAnimatedPreserveAspectRatio(..),
        gTypeSVGAnimatedPreserveAspectRatio)
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

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGAnimatedPreserveAspectRatio.baseVal Mozilla SVGAnimatedPreserveAspectRatio.baseVal documentation> 
getBaseVal ::
           (MonadDOM m) =>
             SVGAnimatedPreserveAspectRatio -> m (Maybe SVGPreserveAspectRatio)
getBaseVal self = liftDOM ((self ^. js "baseVal") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGAnimatedPreserveAspectRatio.baseVal Mozilla SVGAnimatedPreserveAspectRatio.baseVal documentation> 
getBaseValUnsafe ::
                 (MonadDOM m, HasCallStack) =>
                   SVGAnimatedPreserveAspectRatio -> m SVGPreserveAspectRatio
getBaseValUnsafe self
  = liftDOM
      (((self ^. js "baseVal") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGAnimatedPreserveAspectRatio.baseVal Mozilla SVGAnimatedPreserveAspectRatio.baseVal documentation> 
getBaseValUnchecked ::
                    (MonadDOM m) =>
                      SVGAnimatedPreserveAspectRatio -> m SVGPreserveAspectRatio
getBaseValUnchecked self
  = liftDOM ((self ^. js "baseVal") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGAnimatedPreserveAspectRatio.animVal Mozilla SVGAnimatedPreserveAspectRatio.animVal documentation> 
getAnimVal ::
           (MonadDOM m) =>
             SVGAnimatedPreserveAspectRatio -> m (Maybe SVGPreserveAspectRatio)
getAnimVal self = liftDOM ((self ^. js "animVal") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGAnimatedPreserveAspectRatio.animVal Mozilla SVGAnimatedPreserveAspectRatio.animVal documentation> 
getAnimValUnsafe ::
                 (MonadDOM m, HasCallStack) =>
                   SVGAnimatedPreserveAspectRatio -> m SVGPreserveAspectRatio
getAnimValUnsafe self
  = liftDOM
      (((self ^. js "animVal") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGAnimatedPreserveAspectRatio.animVal Mozilla SVGAnimatedPreserveAspectRatio.animVal documentation> 
getAnimValUnchecked ::
                    (MonadDOM m) =>
                      SVGAnimatedPreserveAspectRatio -> m SVGPreserveAspectRatio
getAnimValUnchecked self
  = liftDOM ((self ^. js "animVal") >>= fromJSValUnchecked)
