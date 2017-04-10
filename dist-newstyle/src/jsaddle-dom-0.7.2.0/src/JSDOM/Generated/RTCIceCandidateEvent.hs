{-# LANGUAGE PatternSynonyms #-}
-- For HasCallStack compatibility
{-# LANGUAGE ImplicitParams, ConstraintKinds, KindSignatures #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module JSDOM.Generated.RTCIceCandidateEvent
       (getCandidate, getCandidateUnsafe, getCandidateUnchecked,
        RTCIceCandidateEvent(..), gTypeRTCIceCandidateEvent)
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

-- | <https://developer.mozilla.org/en-US/docs/Web/API/RTCIceCandidateEvent.candidate Mozilla RTCIceCandidateEvent.candidate documentation> 
getCandidate ::
             (MonadDOM m) => RTCIceCandidateEvent -> m (Maybe RTCIceCandidate)
getCandidate self
  = liftDOM ((self ^. js "candidate") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/RTCIceCandidateEvent.candidate Mozilla RTCIceCandidateEvent.candidate documentation> 
getCandidateUnsafe ::
                   (MonadDOM m, HasCallStack) =>
                     RTCIceCandidateEvent -> m RTCIceCandidate
getCandidateUnsafe self
  = liftDOM
      (((self ^. js "candidate") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/RTCIceCandidateEvent.candidate Mozilla RTCIceCandidateEvent.candidate documentation> 
getCandidateUnchecked ::
                      (MonadDOM m) => RTCIceCandidateEvent -> m RTCIceCandidate
getCandidateUnchecked self
  = liftDOM ((self ^. js "candidate") >>= fromJSValUnchecked)
