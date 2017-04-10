{-# LANGUAGE PatternSynonyms #-}
-- For HasCallStack compatibility
{-# LANGUAGE ImplicitParams, ConstraintKinds, KindSignatures #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module JSDOM.Generated.DOMSettableTokenList
       (_get, _get_, _getUnsafe, _getUnchecked, setValue, getValue,
        DOMSettableTokenList(..), gTypeDOMSettableTokenList)
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

-- | <https://developer.mozilla.org/en-US/docs/Web/API/DOMSettableTokenList._get Mozilla DOMSettableTokenList._get documentation> 
_get ::
     (MonadDOM m, FromJSString result) =>
       DOMSettableTokenList -> Word -> m (Maybe result)
_get self index
  = liftDOM
      ((self ^. jsf "_get" [toJSVal index]) >>= fromMaybeJSString)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/DOMSettableTokenList._get Mozilla DOMSettableTokenList._get documentation> 
_get_ :: (MonadDOM m) => DOMSettableTokenList -> Word -> m ()
_get_ self index
  = liftDOM (void (self ^. jsf "_get" [toJSVal index]))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/DOMSettableTokenList._get Mozilla DOMSettableTokenList._get documentation> 
_getUnsafe ::
           (MonadDOM m, HasCallStack, FromJSString result) =>
             DOMSettableTokenList -> Word -> m result
_getUnsafe self index
  = liftDOM
      (((self ^. jsf "_get" [toJSVal index]) >>= fromMaybeJSString) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/DOMSettableTokenList._get Mozilla DOMSettableTokenList._get documentation> 
_getUnchecked ::
              (MonadDOM m, FromJSString result) =>
                DOMSettableTokenList -> Word -> m result
_getUnchecked self index
  = liftDOM
      ((self ^. jsf "_get" [toJSVal index]) >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/DOMSettableTokenList.value Mozilla DOMSettableTokenList.value documentation> 
setValue ::
         (MonadDOM m, ToJSString val) => DOMSettableTokenList -> val -> m ()
setValue self val = liftDOM (self ^. jss "value" (toJSVal val))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/DOMSettableTokenList.value Mozilla DOMSettableTokenList.value documentation> 
getValue ::
         (MonadDOM m, FromJSString result) =>
           DOMSettableTokenList -> m result
getValue self
  = liftDOM ((self ^. js "value") >>= fromJSValUnchecked)
