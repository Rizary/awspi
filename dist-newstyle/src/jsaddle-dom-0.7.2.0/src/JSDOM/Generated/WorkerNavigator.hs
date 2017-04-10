{-# LANGUAGE PatternSynonyms #-}
-- For HasCallStack compatibility
{-# LANGUAGE ImplicitParams, ConstraintKinds, KindSignatures #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module JSDOM.Generated.WorkerNavigator
       (getWebkitTemporaryStorage, getWebkitTemporaryStorageUnsafe,
        getWebkitTemporaryStorageUnchecked, getWebkitPersistentStorage,
        getWebkitPersistentStorageUnsafe,
        getWebkitPersistentStorageUnchecked, getAppName, getAppVersion,
        getPlatform, getUserAgent, getOnLine, WorkerNavigator(..),
        gTypeWorkerNavigator)
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

-- | <https://developer.mozilla.org/en-US/docs/Web/API/WorkerNavigator.webkitTemporaryStorage Mozilla WorkerNavigator.webkitTemporaryStorage documentation> 
getWebkitTemporaryStorage ::
                          (MonadDOM m) => WorkerNavigator -> m (Maybe StorageQuota)
getWebkitTemporaryStorage self
  = liftDOM ((self ^. js "webkitTemporaryStorage") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/WorkerNavigator.webkitTemporaryStorage Mozilla WorkerNavigator.webkitTemporaryStorage documentation> 
getWebkitTemporaryStorageUnsafe ::
                                (MonadDOM m, HasCallStack) => WorkerNavigator -> m StorageQuota
getWebkitTemporaryStorageUnsafe self
  = liftDOM
      (((self ^. js "webkitTemporaryStorage") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/WorkerNavigator.webkitTemporaryStorage Mozilla WorkerNavigator.webkitTemporaryStorage documentation> 
getWebkitTemporaryStorageUnchecked ::
                                   (MonadDOM m) => WorkerNavigator -> m StorageQuota
getWebkitTemporaryStorageUnchecked self
  = liftDOM
      ((self ^. js "webkitTemporaryStorage") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/WorkerNavigator.webkitPersistentStorage Mozilla WorkerNavigator.webkitPersistentStorage documentation> 
getWebkitPersistentStorage ::
                           (MonadDOM m) => WorkerNavigator -> m (Maybe StorageQuota)
getWebkitPersistentStorage self
  = liftDOM ((self ^. js "webkitPersistentStorage") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/WorkerNavigator.webkitPersistentStorage Mozilla WorkerNavigator.webkitPersistentStorage documentation> 
getWebkitPersistentStorageUnsafe ::
                                 (MonadDOM m, HasCallStack) => WorkerNavigator -> m StorageQuota
getWebkitPersistentStorageUnsafe self
  = liftDOM
      (((self ^. js "webkitPersistentStorage") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/WorkerNavigator.webkitPersistentStorage Mozilla WorkerNavigator.webkitPersistentStorage documentation> 
getWebkitPersistentStorageUnchecked ::
                                    (MonadDOM m) => WorkerNavigator -> m StorageQuota
getWebkitPersistentStorageUnchecked self
  = liftDOM
      ((self ^. js "webkitPersistentStorage") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/WorkerNavigator.appName Mozilla WorkerNavigator.appName documentation> 
getAppName ::
           (MonadDOM m, FromJSString result) => WorkerNavigator -> m result
getAppName self
  = liftDOM ((self ^. js "appName") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/WorkerNavigator.appVersion Mozilla WorkerNavigator.appVersion documentation> 
getAppVersion ::
              (MonadDOM m, FromJSString result) => WorkerNavigator -> m result
getAppVersion self
  = liftDOM ((self ^. js "appVersion") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/WorkerNavigator.platform Mozilla WorkerNavigator.platform documentation> 
getPlatform ::
            (MonadDOM m, FromJSString result) => WorkerNavigator -> m result
getPlatform self
  = liftDOM ((self ^. js "platform") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/WorkerNavigator.userAgent Mozilla WorkerNavigator.userAgent documentation> 
getUserAgent ::
             (MonadDOM m, FromJSString result) => WorkerNavigator -> m result
getUserAgent self
  = liftDOM ((self ^. js "userAgent") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/WorkerNavigator.onLine Mozilla WorkerNavigator.onLine documentation> 
getOnLine :: (MonadDOM m) => WorkerNavigator -> m Bool
getOnLine self = liftDOM ((self ^. js "onLine") >>= valToBool)
