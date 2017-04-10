{-# LANGUAGE PatternSynonyms #-}
-- For HasCallStack compatibility
{-# LANGUAGE ImplicitParams, ConstraintKinds, KindSignatures #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module JSDOM.Generated.WindowBase64
       (atob, atob_, btoa, btoa_, WindowBase64(..), gTypeWindowBase64)
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

-- | <https://developer.mozilla.org/en-US/docs/Web/API/WindowBase64.atob Mozilla WindowBase64.atob documentation> 
atob ::
     (MonadDOM m, ToJSString string, FromJSString result) =>
       WindowBase64 -> string -> m result
atob self string
  = liftDOM
      ((self ^. jsf "atob" [toJSVal string]) >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/WindowBase64.atob Mozilla WindowBase64.atob documentation> 
atob_ ::
      (MonadDOM m, ToJSString string) => WindowBase64 -> string -> m ()
atob_ self string
  = liftDOM (void (self ^. jsf "atob" [toJSVal string]))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/WindowBase64.btoa Mozilla WindowBase64.btoa documentation> 
btoa ::
     (MonadDOM m, ToJSString string, FromJSString result) =>
       WindowBase64 -> string -> m result
btoa self string
  = liftDOM
      ((self ^. jsf "btoa" [toJSVal string]) >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/WindowBase64.btoa Mozilla WindowBase64.btoa documentation> 
btoa_ ::
      (MonadDOM m, ToJSString string) => WindowBase64 -> string -> m ()
btoa_ self string
  = liftDOM (void (self ^. jsf "btoa" [toJSVal string]))
