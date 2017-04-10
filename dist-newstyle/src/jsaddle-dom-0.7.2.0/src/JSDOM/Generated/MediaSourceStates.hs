{-# LANGUAGE PatternSynonyms #-}
-- For HasCallStack compatibility
{-# LANGUAGE ImplicitParams, ConstraintKinds, KindSignatures #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module JSDOM.Generated.MediaSourceStates
       (getSourceType, getSourceId, getWidth, getWidthUnsafe,
        getWidthUnchecked, getHeight, getHeightUnsafe, getHeightUnchecked,
        getFrameRate, getFrameRateUnsafe, getFrameRateUnchecked,
        getAspectRatio, getAspectRatioUnsafe, getAspectRatioUnchecked,
        getFacingMode, getFacingModeUnsafe, getFacingModeUnchecked,
        getVolume, getVolumeUnsafe, getVolumeUnchecked,
        MediaSourceStates(..), gTypeMediaSourceStates)
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

-- | <https://developer.mozilla.org/en-US/docs/Web/API/MediaSourceStates.sourceType Mozilla MediaSourceStates.sourceType documentation> 
getSourceType ::
              (MonadDOM m) => MediaSourceStates -> m SourceTypeEnum
getSourceType self
  = liftDOM ((self ^. js "sourceType") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/MediaSourceStates.sourceId Mozilla MediaSourceStates.sourceId documentation> 
getSourceId ::
            (MonadDOM m, FromJSString result) => MediaSourceStates -> m result
getSourceId self
  = liftDOM ((self ^. js "sourceId") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/MediaSourceStates.width Mozilla MediaSourceStates.width documentation> 
getWidth :: (MonadDOM m) => MediaSourceStates -> m (Maybe Word)
getWidth self = liftDOM ((self ^. js "width") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/MediaSourceStates.width Mozilla MediaSourceStates.width documentation> 
getWidthUnsafe ::
               (MonadDOM m, HasCallStack) => MediaSourceStates -> m Word
getWidthUnsafe self
  = liftDOM
      (((self ^. js "width") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/MediaSourceStates.width Mozilla MediaSourceStates.width documentation> 
getWidthUnchecked :: (MonadDOM m) => MediaSourceStates -> m Word
getWidthUnchecked self
  = liftDOM ((self ^. js "width") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/MediaSourceStates.height Mozilla MediaSourceStates.height documentation> 
getHeight :: (MonadDOM m) => MediaSourceStates -> m (Maybe Word)
getHeight self = liftDOM ((self ^. js "height") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/MediaSourceStates.height Mozilla MediaSourceStates.height documentation> 
getHeightUnsafe ::
                (MonadDOM m, HasCallStack) => MediaSourceStates -> m Word
getHeightUnsafe self
  = liftDOM
      (((self ^. js "height") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/MediaSourceStates.height Mozilla MediaSourceStates.height documentation> 
getHeightUnchecked :: (MonadDOM m) => MediaSourceStates -> m Word
getHeightUnchecked self
  = liftDOM ((self ^. js "height") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/MediaSourceStates.frameRate Mozilla MediaSourceStates.frameRate documentation> 
getFrameRate ::
             (MonadDOM m) => MediaSourceStates -> m (Maybe Float)
getFrameRate self
  = liftDOM ((self ^. js "frameRate") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/MediaSourceStates.frameRate Mozilla MediaSourceStates.frameRate documentation> 
getFrameRateUnsafe ::
                   (MonadDOM m, HasCallStack) => MediaSourceStates -> m Float
getFrameRateUnsafe self
  = liftDOM
      (((self ^. js "frameRate") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/MediaSourceStates.frameRate Mozilla MediaSourceStates.frameRate documentation> 
getFrameRateUnchecked ::
                      (MonadDOM m) => MediaSourceStates -> m Float
getFrameRateUnchecked self
  = liftDOM ((self ^. js "frameRate") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/MediaSourceStates.aspectRatio Mozilla MediaSourceStates.aspectRatio documentation> 
getAspectRatio ::
               (MonadDOM m) => MediaSourceStates -> m (Maybe Float)
getAspectRatio self
  = liftDOM ((self ^. js "aspectRatio") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/MediaSourceStates.aspectRatio Mozilla MediaSourceStates.aspectRatio documentation> 
getAspectRatioUnsafe ::
                     (MonadDOM m, HasCallStack) => MediaSourceStates -> m Float
getAspectRatioUnsafe self
  = liftDOM
      (((self ^. js "aspectRatio") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/MediaSourceStates.aspectRatio Mozilla MediaSourceStates.aspectRatio documentation> 
getAspectRatioUnchecked ::
                        (MonadDOM m) => MediaSourceStates -> m Float
getAspectRatioUnchecked self
  = liftDOM ((self ^. js "aspectRatio") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/MediaSourceStates.facingMode Mozilla MediaSourceStates.facingMode documentation> 
getFacingMode ::
              (MonadDOM m) => MediaSourceStates -> m (Maybe VideoFacingModeEnum)
getFacingMode self
  = liftDOM ((self ^. js "facingMode") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/MediaSourceStates.facingMode Mozilla MediaSourceStates.facingMode documentation> 
getFacingModeUnsafe ::
                    (MonadDOM m, HasCallStack) =>
                      MediaSourceStates -> m VideoFacingModeEnum
getFacingModeUnsafe self
  = liftDOM
      (((self ^. js "facingMode") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/MediaSourceStates.facingMode Mozilla MediaSourceStates.facingMode documentation> 
getFacingModeUnchecked ::
                       (MonadDOM m) => MediaSourceStates -> m VideoFacingModeEnum
getFacingModeUnchecked self
  = liftDOM ((self ^. js "facingMode") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/MediaSourceStates.volume Mozilla MediaSourceStates.volume documentation> 
getVolume :: (MonadDOM m) => MediaSourceStates -> m (Maybe Word)
getVolume self = liftDOM ((self ^. js "volume") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/MediaSourceStates.volume Mozilla MediaSourceStates.volume documentation> 
getVolumeUnsafe ::
                (MonadDOM m, HasCallStack) => MediaSourceStates -> m Word
getVolumeUnsafe self
  = liftDOM
      (((self ^. js "volume") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/MediaSourceStates.volume Mozilla MediaSourceStates.volume documentation> 
getVolumeUnchecked :: (MonadDOM m) => MediaSourceStates -> m Word
getVolumeUnchecked self
  = liftDOM ((self ^. js "volume") >>= fromJSValUnchecked)
