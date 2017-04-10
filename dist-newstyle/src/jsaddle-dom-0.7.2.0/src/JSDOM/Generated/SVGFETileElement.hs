{-# LANGUAGE PatternSynonyms #-}
-- For HasCallStack compatibility
{-# LANGUAGE ImplicitParams, ConstraintKinds, KindSignatures #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module JSDOM.Generated.SVGFETileElement
       (getIn1, getIn1Unsafe, getIn1Unchecked, SVGFETileElement(..),
        gTypeSVGFETileElement)
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

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGFETileElement.in1 Mozilla SVGFETileElement.in1 documentation> 
getIn1 ::
       (MonadDOM m) => SVGFETileElement -> m (Maybe SVGAnimatedString)
getIn1 self = liftDOM ((self ^. js "in1") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGFETileElement.in1 Mozilla SVGFETileElement.in1 documentation> 
getIn1Unsafe ::
             (MonadDOM m, HasCallStack) =>
               SVGFETileElement -> m SVGAnimatedString
getIn1Unsafe self
  = liftDOM
      (((self ^. js "in1") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGFETileElement.in1 Mozilla SVGFETileElement.in1 documentation> 
getIn1Unchecked ::
                (MonadDOM m) => SVGFETileElement -> m SVGAnimatedString
getIn1Unchecked self
  = liftDOM ((self ^. js "in1") >>= fromJSValUnchecked)
