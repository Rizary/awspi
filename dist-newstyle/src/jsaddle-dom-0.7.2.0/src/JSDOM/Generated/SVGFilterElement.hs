{-# LANGUAGE PatternSynonyms #-}
-- For HasCallStack compatibility
{-# LANGUAGE ImplicitParams, ConstraintKinds, KindSignatures #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module JSDOM.Generated.SVGFilterElement
       (setFilterRes, getFilterUnits, getFilterUnitsUnsafe,
        getFilterUnitsUnchecked, getPrimitiveUnits,
        getPrimitiveUnitsUnsafe, getPrimitiveUnitsUnchecked, getX,
        getXUnsafe, getXUnchecked, getY, getYUnsafe, getYUnchecked,
        getWidth, getWidthUnsafe, getWidthUnchecked, getHeight,
        getHeightUnsafe, getHeightUnchecked, getFilterResX,
        getFilterResXUnsafe, getFilterResXUnchecked, getFilterResY,
        getFilterResYUnsafe, getFilterResYUnchecked, SVGFilterElement(..),
        gTypeSVGFilterElement)
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

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGFilterElement.setFilterRes Mozilla SVGFilterElement.setFilterRes documentation> 
setFilterRes ::
             (MonadDOM m) => SVGFilterElement -> Word -> Word -> m ()
setFilterRes self filterResX filterResY
  = liftDOM
      (void
         (self ^. jsf "setFilterRes"
            [toJSVal filterResX, toJSVal filterResY]))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGFilterElement.filterUnits Mozilla SVGFilterElement.filterUnits documentation> 
getFilterUnits ::
               (MonadDOM m) =>
                 SVGFilterElement -> m (Maybe SVGAnimatedEnumeration)
getFilterUnits self
  = liftDOM ((self ^. js "filterUnits") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGFilterElement.filterUnits Mozilla SVGFilterElement.filterUnits documentation> 
getFilterUnitsUnsafe ::
                     (MonadDOM m, HasCallStack) =>
                       SVGFilterElement -> m SVGAnimatedEnumeration
getFilterUnitsUnsafe self
  = liftDOM
      (((self ^. js "filterUnits") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGFilterElement.filterUnits Mozilla SVGFilterElement.filterUnits documentation> 
getFilterUnitsUnchecked ::
                        (MonadDOM m) => SVGFilterElement -> m SVGAnimatedEnumeration
getFilterUnitsUnchecked self
  = liftDOM ((self ^. js "filterUnits") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGFilterElement.primitiveUnits Mozilla SVGFilterElement.primitiveUnits documentation> 
getPrimitiveUnits ::
                  (MonadDOM m) =>
                    SVGFilterElement -> m (Maybe SVGAnimatedEnumeration)
getPrimitiveUnits self
  = liftDOM ((self ^. js "primitiveUnits") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGFilterElement.primitiveUnits Mozilla SVGFilterElement.primitiveUnits documentation> 
getPrimitiveUnitsUnsafe ::
                        (MonadDOM m, HasCallStack) =>
                          SVGFilterElement -> m SVGAnimatedEnumeration
getPrimitiveUnitsUnsafe self
  = liftDOM
      (((self ^. js "primitiveUnits") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGFilterElement.primitiveUnits Mozilla SVGFilterElement.primitiveUnits documentation> 
getPrimitiveUnitsUnchecked ::
                           (MonadDOM m) => SVGFilterElement -> m SVGAnimatedEnumeration
getPrimitiveUnitsUnchecked self
  = liftDOM ((self ^. js "primitiveUnits") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGFilterElement.x Mozilla SVGFilterElement.x documentation> 
getX ::
     (MonadDOM m) => SVGFilterElement -> m (Maybe SVGAnimatedLength)
getX self = liftDOM ((self ^. js "x") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGFilterElement.x Mozilla SVGFilterElement.x documentation> 
getXUnsafe ::
           (MonadDOM m, HasCallStack) =>
             SVGFilterElement -> m SVGAnimatedLength
getXUnsafe self
  = liftDOM
      (((self ^. js "x") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGFilterElement.x Mozilla SVGFilterElement.x documentation> 
getXUnchecked ::
              (MonadDOM m) => SVGFilterElement -> m SVGAnimatedLength
getXUnchecked self
  = liftDOM ((self ^. js "x") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGFilterElement.y Mozilla SVGFilterElement.y documentation> 
getY ::
     (MonadDOM m) => SVGFilterElement -> m (Maybe SVGAnimatedLength)
getY self = liftDOM ((self ^. js "y") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGFilterElement.y Mozilla SVGFilterElement.y documentation> 
getYUnsafe ::
           (MonadDOM m, HasCallStack) =>
             SVGFilterElement -> m SVGAnimatedLength
getYUnsafe self
  = liftDOM
      (((self ^. js "y") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGFilterElement.y Mozilla SVGFilterElement.y documentation> 
getYUnchecked ::
              (MonadDOM m) => SVGFilterElement -> m SVGAnimatedLength
getYUnchecked self
  = liftDOM ((self ^. js "y") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGFilterElement.width Mozilla SVGFilterElement.width documentation> 
getWidth ::
         (MonadDOM m) => SVGFilterElement -> m (Maybe SVGAnimatedLength)
getWidth self = liftDOM ((self ^. js "width") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGFilterElement.width Mozilla SVGFilterElement.width documentation> 
getWidthUnsafe ::
               (MonadDOM m, HasCallStack) =>
                 SVGFilterElement -> m SVGAnimatedLength
getWidthUnsafe self
  = liftDOM
      (((self ^. js "width") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGFilterElement.width Mozilla SVGFilterElement.width documentation> 
getWidthUnchecked ::
                  (MonadDOM m) => SVGFilterElement -> m SVGAnimatedLength
getWidthUnchecked self
  = liftDOM ((self ^. js "width") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGFilterElement.height Mozilla SVGFilterElement.height documentation> 
getHeight ::
          (MonadDOM m) => SVGFilterElement -> m (Maybe SVGAnimatedLength)
getHeight self = liftDOM ((self ^. js "height") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGFilterElement.height Mozilla SVGFilterElement.height documentation> 
getHeightUnsafe ::
                (MonadDOM m, HasCallStack) =>
                  SVGFilterElement -> m SVGAnimatedLength
getHeightUnsafe self
  = liftDOM
      (((self ^. js "height") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGFilterElement.height Mozilla SVGFilterElement.height documentation> 
getHeightUnchecked ::
                   (MonadDOM m) => SVGFilterElement -> m SVGAnimatedLength
getHeightUnchecked self
  = liftDOM ((self ^. js "height") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGFilterElement.filterResX Mozilla SVGFilterElement.filterResX documentation> 
getFilterResX ::
              (MonadDOM m) => SVGFilterElement -> m (Maybe SVGAnimatedInteger)
getFilterResX self
  = liftDOM ((self ^. js "filterResX") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGFilterElement.filterResX Mozilla SVGFilterElement.filterResX documentation> 
getFilterResXUnsafe ::
                    (MonadDOM m, HasCallStack) =>
                      SVGFilterElement -> m SVGAnimatedInteger
getFilterResXUnsafe self
  = liftDOM
      (((self ^. js "filterResX") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGFilterElement.filterResX Mozilla SVGFilterElement.filterResX documentation> 
getFilterResXUnchecked ::
                       (MonadDOM m) => SVGFilterElement -> m SVGAnimatedInteger
getFilterResXUnchecked self
  = liftDOM ((self ^. js "filterResX") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGFilterElement.filterResY Mozilla SVGFilterElement.filterResY documentation> 
getFilterResY ::
              (MonadDOM m) => SVGFilterElement -> m (Maybe SVGAnimatedInteger)
getFilterResY self
  = liftDOM ((self ^. js "filterResY") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGFilterElement.filterResY Mozilla SVGFilterElement.filterResY documentation> 
getFilterResYUnsafe ::
                    (MonadDOM m, HasCallStack) =>
                      SVGFilterElement -> m SVGAnimatedInteger
getFilterResYUnsafe self
  = liftDOM
      (((self ^. js "filterResY") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGFilterElement.filterResY Mozilla SVGFilterElement.filterResY documentation> 
getFilterResYUnchecked ::
                       (MonadDOM m) => SVGFilterElement -> m SVGAnimatedInteger
getFilterResYUnchecked self
  = liftDOM ((self ^. js "filterResY") >>= fromJSValUnchecked)
