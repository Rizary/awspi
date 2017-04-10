{-# LANGUAGE PatternSynonyms #-}
-- For HasCallStack compatibility
{-# LANGUAGE ImplicitParams, ConstraintKinds, KindSignatures #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module JSDOM.Generated.HTMLDataListElement
       (getOptions, getOptionsUnsafe, getOptionsUnchecked,
        HTMLDataListElement(..), gTypeHTMLDataListElement)
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

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLDataListElement.options Mozilla HTMLDataListElement.options documentation> 
getOptions ::
           (MonadDOM m) => HTMLDataListElement -> m (Maybe HTMLCollection)
getOptions self = liftDOM ((self ^. js "options") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLDataListElement.options Mozilla HTMLDataListElement.options documentation> 
getOptionsUnsafe ::
                 (MonadDOM m, HasCallStack) =>
                   HTMLDataListElement -> m HTMLCollection
getOptionsUnsafe self
  = liftDOM
      (((self ^. js "options") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLDataListElement.options Mozilla HTMLDataListElement.options documentation> 
getOptionsUnchecked ::
                    (MonadDOM m) => HTMLDataListElement -> m HTMLCollection
getOptionsUnchecked self
  = liftDOM ((self ^. js "options") >>= fromJSValUnchecked)
