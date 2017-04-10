{-# LANGUAGE PatternSynonyms #-}
-- For HasCallStack compatibility
{-# LANGUAGE ImplicitParams, ConstraintKinds, KindSignatures #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module JSDOM.Generated.PositionErrorCallback
       (newPositionErrorCallback, newPositionErrorCallbackSync,
        newPositionErrorCallbackAsync, PositionErrorCallback)
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

-- | <https://developer.mozilla.org/en-US/docs/Web/API/PositionErrorCallback Mozilla PositionErrorCallback documentation> 
newPositionErrorCallback ::
                         (MonadDOM m) =>
                           (Maybe PositionError -> JSM ()) -> m PositionErrorCallback
newPositionErrorCallback callback
  = liftDOM
      (PositionErrorCallback . Callback <$>
         function
           (\ _ _ [error] -> fromJSVal error >>= \ error' -> callback error'))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/PositionErrorCallback Mozilla PositionErrorCallback documentation> 
newPositionErrorCallbackSync ::
                             (MonadDOM m) =>
                               (Maybe PositionError -> JSM ()) -> m PositionErrorCallback
newPositionErrorCallbackSync callback
  = liftDOM
      (PositionErrorCallback . Callback <$>
         function
           (\ _ _ [error] -> fromJSVal error >>= \ error' -> callback error'))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/PositionErrorCallback Mozilla PositionErrorCallback documentation> 
newPositionErrorCallbackAsync ::
                              (MonadDOM m) =>
                                (Maybe PositionError -> JSM ()) -> m PositionErrorCallback
newPositionErrorCallbackAsync callback
  = liftDOM
      (PositionErrorCallback . Callback <$>
         function
           (\ _ _ [error] -> fromJSVal error >>= \ error' -> callback error'))
