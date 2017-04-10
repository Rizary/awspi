{-# LANGUAGE PatternSynonyms #-}
-- For HasCallStack compatibility
{-# LANGUAGE ImplicitParams, ConstraintKinds, KindSignatures #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module JSDOM.Generated.SQLTransaction
       (executeSql, SQLTransaction(..), gTypeSQLTransaction) where
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

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SQLTransaction.executeSql Mozilla SQLTransaction.executeSql documentation> 
executeSql ::
           (MonadDOM m, ToJSString sqlStatement, IsObjectArray arguments) =>
             SQLTransaction ->
               sqlStatement ->
                 Maybe arguments ->
                   Maybe SQLStatementCallback ->
                     Maybe SQLStatementErrorCallback -> m ()
executeSql self sqlStatement arguments callback errorCallback
  = liftDOM
      (void
         (self ^. jsf "executeSql"
            [toJSVal sqlStatement, toJSVal arguments, toJSVal callback,
             toJSVal errorCallback]))
