{-# LANGUAGE PatternSynonyms #-}
-- For HasCallStack compatibility
{-# LANGUAGE ImplicitParams, ConstraintKinds, KindSignatures #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module JSDOM.Generated.AudioProcessingEvent
       (getPlaybackTime, getInputBuffer, getInputBufferUnsafe,
        getInputBufferUnchecked, getOutputBuffer, getOutputBufferUnsafe,
        getOutputBufferUnchecked, AudioProcessingEvent(..),
        gTypeAudioProcessingEvent)
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

-- | <https://developer.mozilla.org/en-US/docs/Web/API/AudioProcessingEvent.playbackTime Mozilla AudioProcessingEvent.playbackTime documentation> 
getPlaybackTime :: (MonadDOM m) => AudioProcessingEvent -> m Double
getPlaybackTime self
  = liftDOM ((self ^. js "playbackTime") >>= valToNumber)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/AudioProcessingEvent.inputBuffer Mozilla AudioProcessingEvent.inputBuffer documentation> 
getInputBuffer ::
               (MonadDOM m) => AudioProcessingEvent -> m (Maybe AudioBuffer)
getInputBuffer self
  = liftDOM ((self ^. js "inputBuffer") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/AudioProcessingEvent.inputBuffer Mozilla AudioProcessingEvent.inputBuffer documentation> 
getInputBufferUnsafe ::
                     (MonadDOM m, HasCallStack) => AudioProcessingEvent -> m AudioBuffer
getInputBufferUnsafe self
  = liftDOM
      (((self ^. js "inputBuffer") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/AudioProcessingEvent.inputBuffer Mozilla AudioProcessingEvent.inputBuffer documentation> 
getInputBufferUnchecked ::
                        (MonadDOM m) => AudioProcessingEvent -> m AudioBuffer
getInputBufferUnchecked self
  = liftDOM ((self ^. js "inputBuffer") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/AudioProcessingEvent.outputBuffer Mozilla AudioProcessingEvent.outputBuffer documentation> 
getOutputBuffer ::
                (MonadDOM m) => AudioProcessingEvent -> m (Maybe AudioBuffer)
getOutputBuffer self
  = liftDOM ((self ^. js "outputBuffer") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/AudioProcessingEvent.outputBuffer Mozilla AudioProcessingEvent.outputBuffer documentation> 
getOutputBufferUnsafe ::
                      (MonadDOM m, HasCallStack) => AudioProcessingEvent -> m AudioBuffer
getOutputBufferUnsafe self
  = liftDOM
      (((self ^. js "outputBuffer") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/AudioProcessingEvent.outputBuffer Mozilla AudioProcessingEvent.outputBuffer documentation> 
getOutputBufferUnchecked ::
                         (MonadDOM m) => AudioProcessingEvent -> m AudioBuffer
getOutputBufferUnchecked self
  = liftDOM ((self ^. js "outputBuffer") >>= fromJSValUnchecked)
