{-# LANGUAGE PatternSynonyms #-}
-- For HasCallStack compatibility
{-# LANGUAGE ImplicitParams, ConstraintKinds, KindSignatures #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module JSDOM.Generated.RTCConfiguration
       (getIceServers, getIceTransports, getRequestIdentity,
        RTCConfiguration(..), gTypeRTCConfiguration)
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

-- | <https://developer.mozilla.org/en-US/docs/Web/API/RTCConfiguration.iceServers Mozilla RTCConfiguration.iceServers documentation> 
getIceServers ::
              (MonadDOM m) => RTCConfiguration -> m [Maybe RTCIceServer]
getIceServers self
  = liftDOM ((self ^. js "iceServers") >>= fromJSArray)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/RTCConfiguration.iceTransports Mozilla RTCConfiguration.iceTransports documentation> 
getIceTransports ::
                 (MonadDOM m) => RTCConfiguration -> m RTCIceTransportsEnum
getIceTransports self
  = liftDOM ((self ^. js "iceTransports") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/RTCConfiguration.requestIdentity Mozilla RTCConfiguration.requestIdentity documentation> 
getRequestIdentity ::
                   (MonadDOM m) => RTCConfiguration -> m RTCIdentityOptionEnum
getRequestIdentity self
  = liftDOM ((self ^. js "requestIdentity") >>= fromJSValUnchecked)
