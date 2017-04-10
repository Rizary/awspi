{-# LANGUAGE PatternSynonyms #-}
-- For HasCallStack compatibility
{-# LANGUAGE ImplicitParams, ConstraintKinds, KindSignatures #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module JSDOM.Generated.ErrorEvent
       (getMessage, getFilename, getLineno, getColno, ErrorEvent(..),
        gTypeErrorEvent)
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

-- | <https://developer.mozilla.org/en-US/docs/Web/API/ErrorEvent.message Mozilla ErrorEvent.message documentation> 
getMessage ::
           (MonadDOM m, FromJSString result) => ErrorEvent -> m result
getMessage self
  = liftDOM ((self ^. js "message") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/ErrorEvent.filename Mozilla ErrorEvent.filename documentation> 
getFilename ::
            (MonadDOM m, FromJSString result) => ErrorEvent -> m result
getFilename self
  = liftDOM ((self ^. js "filename") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/ErrorEvent.lineno Mozilla ErrorEvent.lineno documentation> 
getLineno :: (MonadDOM m) => ErrorEvent -> m Word
getLineno self
  = liftDOM (round <$> ((self ^. js "lineno") >>= valToNumber))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/ErrorEvent.colno Mozilla ErrorEvent.colno documentation> 
getColno :: (MonadDOM m) => ErrorEvent -> m Word
getColno self
  = liftDOM (round <$> ((self ^. js "colno") >>= valToNumber))
