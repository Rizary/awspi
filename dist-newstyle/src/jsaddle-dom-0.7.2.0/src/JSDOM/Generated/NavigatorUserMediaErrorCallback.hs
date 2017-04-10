{-# LANGUAGE PatternSynonyms #-}
-- For HasCallStack compatibility
{-# LANGUAGE ImplicitParams, ConstraintKinds, KindSignatures #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module JSDOM.Generated.NavigatorUserMediaErrorCallback
       (newNavigatorUserMediaErrorCallback,
        newNavigatorUserMediaErrorCallbackSync,
        newNavigatorUserMediaErrorCallbackAsync,
        NavigatorUserMediaErrorCallback)
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

-- | <https://developer.mozilla.org/en-US/docs/Web/API/NavigatorUserMediaErrorCallback Mozilla NavigatorUserMediaErrorCallback documentation> 
newNavigatorUserMediaErrorCallback ::
                                   (MonadDOM m) =>
                                     (Maybe NavigatorUserMediaError -> JSM ()) ->
                                       m NavigatorUserMediaErrorCallback
newNavigatorUserMediaErrorCallback callback
  = liftDOM
      (NavigatorUserMediaErrorCallback . Callback <$>
         function
           (\ _ _ [error] -> fromJSVal error >>= \ error' -> callback error'))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/NavigatorUserMediaErrorCallback Mozilla NavigatorUserMediaErrorCallback documentation> 
newNavigatorUserMediaErrorCallbackSync ::
                                       (MonadDOM m) =>
                                         (Maybe NavigatorUserMediaError -> JSM ()) ->
                                           m NavigatorUserMediaErrorCallback
newNavigatorUserMediaErrorCallbackSync callback
  = liftDOM
      (NavigatorUserMediaErrorCallback . Callback <$>
         function
           (\ _ _ [error] -> fromJSVal error >>= \ error' -> callback error'))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/NavigatorUserMediaErrorCallback Mozilla NavigatorUserMediaErrorCallback documentation> 
newNavigatorUserMediaErrorCallbackAsync ::
                                        (MonadDOM m) =>
                                          (Maybe NavigatorUserMediaError -> JSM ()) ->
                                            m NavigatorUserMediaErrorCallback
newNavigatorUserMediaErrorCallbackAsync callback
  = liftDOM
      (NavigatorUserMediaErrorCallback . Callback <$>
         function
           (\ _ _ [error] -> fromJSVal error >>= \ error' -> callback error'))
