{-# LANGUAGE PatternSynonyms #-}
-- For HasCallStack compatibility
{-# LANGUAGE ImplicitParams, ConstraintKinds, KindSignatures #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module JSDOM.Generated.DynamicsCompressorNode
       (getThreshold, getThresholdUnsafe, getThresholdUnchecked, getKnee,
        getKneeUnsafe, getKneeUnchecked, getRatio, getRatioUnsafe,
        getRatioUnchecked, getReduction, getReductionUnsafe,
        getReductionUnchecked, getAttack, getAttackUnsafe,
        getAttackUnchecked, getRelease, getReleaseUnsafe,
        getReleaseUnchecked, DynamicsCompressorNode(..),
        gTypeDynamicsCompressorNode)
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

-- | <https://developer.mozilla.org/en-US/docs/Web/API/DynamicsCompressorNode.threshold Mozilla DynamicsCompressorNode.threshold documentation> 
getThreshold ::
             (MonadDOM m) => DynamicsCompressorNode -> m (Maybe AudioParam)
getThreshold self
  = liftDOM ((self ^. js "threshold") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/DynamicsCompressorNode.threshold Mozilla DynamicsCompressorNode.threshold documentation> 
getThresholdUnsafe ::
                   (MonadDOM m, HasCallStack) =>
                     DynamicsCompressorNode -> m AudioParam
getThresholdUnsafe self
  = liftDOM
      (((self ^. js "threshold") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/DynamicsCompressorNode.threshold Mozilla DynamicsCompressorNode.threshold documentation> 
getThresholdUnchecked ::
                      (MonadDOM m) => DynamicsCompressorNode -> m AudioParam
getThresholdUnchecked self
  = liftDOM ((self ^. js "threshold") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/DynamicsCompressorNode.knee Mozilla DynamicsCompressorNode.knee documentation> 
getKnee ::
        (MonadDOM m) => DynamicsCompressorNode -> m (Maybe AudioParam)
getKnee self = liftDOM ((self ^. js "knee") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/DynamicsCompressorNode.knee Mozilla DynamicsCompressorNode.knee documentation> 
getKneeUnsafe ::
              (MonadDOM m, HasCallStack) =>
                DynamicsCompressorNode -> m AudioParam
getKneeUnsafe self
  = liftDOM
      (((self ^. js "knee") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/DynamicsCompressorNode.knee Mozilla DynamicsCompressorNode.knee documentation> 
getKneeUnchecked ::
                 (MonadDOM m) => DynamicsCompressorNode -> m AudioParam
getKneeUnchecked self
  = liftDOM ((self ^. js "knee") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/DynamicsCompressorNode.ratio Mozilla DynamicsCompressorNode.ratio documentation> 
getRatio ::
         (MonadDOM m) => DynamicsCompressorNode -> m (Maybe AudioParam)
getRatio self = liftDOM ((self ^. js "ratio") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/DynamicsCompressorNode.ratio Mozilla DynamicsCompressorNode.ratio documentation> 
getRatioUnsafe ::
               (MonadDOM m, HasCallStack) =>
                 DynamicsCompressorNode -> m AudioParam
getRatioUnsafe self
  = liftDOM
      (((self ^. js "ratio") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/DynamicsCompressorNode.ratio Mozilla DynamicsCompressorNode.ratio documentation> 
getRatioUnchecked ::
                  (MonadDOM m) => DynamicsCompressorNode -> m AudioParam
getRatioUnchecked self
  = liftDOM ((self ^. js "ratio") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/DynamicsCompressorNode.reduction Mozilla DynamicsCompressorNode.reduction documentation> 
getReduction ::
             (MonadDOM m) => DynamicsCompressorNode -> m (Maybe AudioParam)
getReduction self
  = liftDOM ((self ^. js "reduction") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/DynamicsCompressorNode.reduction Mozilla DynamicsCompressorNode.reduction documentation> 
getReductionUnsafe ::
                   (MonadDOM m, HasCallStack) =>
                     DynamicsCompressorNode -> m AudioParam
getReductionUnsafe self
  = liftDOM
      (((self ^. js "reduction") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/DynamicsCompressorNode.reduction Mozilla DynamicsCompressorNode.reduction documentation> 
getReductionUnchecked ::
                      (MonadDOM m) => DynamicsCompressorNode -> m AudioParam
getReductionUnchecked self
  = liftDOM ((self ^. js "reduction") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/DynamicsCompressorNode.attack Mozilla DynamicsCompressorNode.attack documentation> 
getAttack ::
          (MonadDOM m) => DynamicsCompressorNode -> m (Maybe AudioParam)
getAttack self = liftDOM ((self ^. js "attack") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/DynamicsCompressorNode.attack Mozilla DynamicsCompressorNode.attack documentation> 
getAttackUnsafe ::
                (MonadDOM m, HasCallStack) =>
                  DynamicsCompressorNode -> m AudioParam
getAttackUnsafe self
  = liftDOM
      (((self ^. js "attack") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/DynamicsCompressorNode.attack Mozilla DynamicsCompressorNode.attack documentation> 
getAttackUnchecked ::
                   (MonadDOM m) => DynamicsCompressorNode -> m AudioParam
getAttackUnchecked self
  = liftDOM ((self ^. js "attack") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/DynamicsCompressorNode.release Mozilla DynamicsCompressorNode.release documentation> 
getRelease ::
           (MonadDOM m) => DynamicsCompressorNode -> m (Maybe AudioParam)
getRelease self = liftDOM ((self ^. js "release") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/DynamicsCompressorNode.release Mozilla DynamicsCompressorNode.release documentation> 
getReleaseUnsafe ::
                 (MonadDOM m, HasCallStack) =>
                   DynamicsCompressorNode -> m AudioParam
getReleaseUnsafe self
  = liftDOM
      (((self ^. js "release") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/DynamicsCompressorNode.release Mozilla DynamicsCompressorNode.release documentation> 
getReleaseUnchecked ::
                    (MonadDOM m) => DynamicsCompressorNode -> m AudioParam
getReleaseUnchecked self
  = liftDOM ((self ^. js "release") >>= fromJSValUnchecked)
