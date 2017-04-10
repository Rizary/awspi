{-# LANGUAGE PatternSynonyms #-}
-- For HasCallStack compatibility
{-# LANGUAGE ImplicitParams, ConstraintKinds, KindSignatures #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module JSDOM.Generated.SVGStopElement
       (getOffset, getOffsetUnsafe, getOffsetUnchecked,
        SVGStopElement(..), gTypeSVGStopElement)
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

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGStopElement.offset Mozilla SVGStopElement.offset documentation> 
getOffset ::
          (MonadDOM m) => SVGStopElement -> m (Maybe SVGAnimatedNumber)
getOffset self = liftDOM ((self ^. js "offset") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGStopElement.offset Mozilla SVGStopElement.offset documentation> 
getOffsetUnsafe ::
                (MonadDOM m, HasCallStack) => SVGStopElement -> m SVGAnimatedNumber
getOffsetUnsafe self
  = liftDOM
      (((self ^. js "offset") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGStopElement.offset Mozilla SVGStopElement.offset documentation> 
getOffsetUnchecked ::
                   (MonadDOM m) => SVGStopElement -> m SVGAnimatedNumber
getOffsetUnchecked self
  = liftDOM ((self ^. js "offset") >>= fromJSValUnchecked)
