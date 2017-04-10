{-# LANGUAGE PatternSynonyms #-}
-- For HasCallStack compatibility
{-# LANGUAGE ImplicitParams, ConstraintKinds, KindSignatures #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module JSDOM.Generated.CSSCharsetRule
       (setEncoding, getEncoding, getEncodingUnsafe, getEncodingUnchecked,
        CSSCharsetRule(..), gTypeCSSCharsetRule)
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

-- | <https://developer.mozilla.org/en-US/docs/Web/API/CSSCharsetRule.encoding Mozilla CSSCharsetRule.encoding documentation> 
setEncoding ::
            (MonadDOM m, ToJSString val) => CSSCharsetRule -> Maybe val -> m ()
setEncoding self val
  = liftDOM (self ^. jss "encoding" (toJSVal val))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/CSSCharsetRule.encoding Mozilla CSSCharsetRule.encoding documentation> 
getEncoding ::
            (MonadDOM m, FromJSString result) =>
              CSSCharsetRule -> m (Maybe result)
getEncoding self
  = liftDOM ((self ^. js "encoding") >>= fromMaybeJSString)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/CSSCharsetRule.encoding Mozilla CSSCharsetRule.encoding documentation> 
getEncodingUnsafe ::
                  (MonadDOM m, HasCallStack, FromJSString result) =>
                    CSSCharsetRule -> m result
getEncodingUnsafe self
  = liftDOM
      (((self ^. js "encoding") >>= fromMaybeJSString) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/CSSCharsetRule.encoding Mozilla CSSCharsetRule.encoding documentation> 
getEncodingUnchecked ::
                     (MonadDOM m, FromJSString result) => CSSCharsetRule -> m result
getEncodingUnchecked self
  = liftDOM ((self ^. js "encoding") >>= fromJSValUnchecked)
