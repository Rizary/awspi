{-# LANGUAGE PatternSynonyms #-}
-- For HasCallStack compatibility
{-# LANGUAGE ImplicitParams, ConstraintKinds, KindSignatures #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module JSDOM.Generated.RTCIceServer
       (getUrls, getUsername, getCredential, RTCIceServer(..),
        gTypeRTCIceServer)
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

-- | <https://developer.mozilla.org/en-US/docs/Web/API/RTCIceServer.urls Mozilla RTCIceServer.urls documentation> 
getUrls ::
        (MonadDOM m, FromJSString result) => RTCIceServer -> m [result]
getUrls self
  = liftDOM ((self ^. js "urls") >>= fromJSArrayUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/RTCIceServer.username Mozilla RTCIceServer.username documentation> 
getUsername ::
            (MonadDOM m, FromJSString result) => RTCIceServer -> m result
getUsername self
  = liftDOM ((self ^. js "username") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/RTCIceServer.credential Mozilla RTCIceServer.credential documentation> 
getCredential ::
              (MonadDOM m, FromJSString result) => RTCIceServer -> m result
getCredential self
  = liftDOM ((self ^. js "credential") >>= fromJSValUnchecked)
