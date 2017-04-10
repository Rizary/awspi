{-# LANGUAGE PatternSynonyms #-}
-- For HasCallStack compatibility
{-# LANGUAGE ImplicitParams, ConstraintKinds, KindSignatures #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module JSDOM.Generated.MediaStreamAudioDestinationNode
       (getStream, getStreamUnsafe, getStreamUnchecked,
        MediaStreamAudioDestinationNode(..),
        gTypeMediaStreamAudioDestinationNode)
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

-- | <https://developer.mozilla.org/en-US/docs/Web/API/MediaStreamAudioDestinationNode.stream Mozilla MediaStreamAudioDestinationNode.stream documentation> 
getStream ::
          (MonadDOM m) =>
            MediaStreamAudioDestinationNode -> m (Maybe MediaStream)
getStream self = liftDOM ((self ^. js "stream") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/MediaStreamAudioDestinationNode.stream Mozilla MediaStreamAudioDestinationNode.stream documentation> 
getStreamUnsafe ::
                (MonadDOM m, HasCallStack) =>
                  MediaStreamAudioDestinationNode -> m MediaStream
getStreamUnsafe self
  = liftDOM
      (((self ^. js "stream") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/MediaStreamAudioDestinationNode.stream Mozilla MediaStreamAudioDestinationNode.stream documentation> 
getStreamUnchecked ::
                   (MonadDOM m) => MediaStreamAudioDestinationNode -> m MediaStream
getStreamUnchecked self
  = liftDOM ((self ^. js "stream") >>= fromJSValUnchecked)
