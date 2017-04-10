{-# LANGUAGE PatternSynonyms #-}
-- For HasCallStack compatibility
{-# LANGUAGE ImplicitParams, ConstraintKinds, KindSignatures #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module JSDOM.Generated.SVGPathSegList
       (clear, initialize, initialize_, initializeUnsafe,
        initializeUnchecked, getItem, getItem_, getItemUnsafe,
        getItemUnchecked, insertItemBefore, insertItemBefore_,
        insertItemBeforeUnsafe, insertItemBeforeUnchecked, replaceItem,
        replaceItem_, replaceItemUnsafe, replaceItemUnchecked, removeItem,
        removeItem_, removeItemUnsafe, removeItemUnchecked, appendItem,
        appendItem_, appendItemUnsafe, appendItemUnchecked,
        getNumberOfItems, SVGPathSegList(..), gTypeSVGPathSegList)
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

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGPathSegList.clear Mozilla SVGPathSegList.clear documentation> 
clear :: (MonadDOM m) => SVGPathSegList -> m ()
clear self = liftDOM (void (self ^. jsf "clear" ()))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGPathSegList.initialize Mozilla SVGPathSegList.initialize documentation> 
initialize ::
           (MonadDOM m, IsSVGPathSeg newItem) =>
             SVGPathSegList -> Maybe newItem -> m (Maybe SVGPathSeg)
initialize self newItem
  = liftDOM
      ((self ^. jsf "initialize" [toJSVal newItem]) >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGPathSegList.initialize Mozilla SVGPathSegList.initialize documentation> 
initialize_ ::
            (MonadDOM m, IsSVGPathSeg newItem) =>
              SVGPathSegList -> Maybe newItem -> m ()
initialize_ self newItem
  = liftDOM (void (self ^. jsf "initialize" [toJSVal newItem]))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGPathSegList.initialize Mozilla SVGPathSegList.initialize documentation> 
initializeUnsafe ::
                 (MonadDOM m, IsSVGPathSeg newItem, HasCallStack) =>
                   SVGPathSegList -> Maybe newItem -> m SVGPathSeg
initializeUnsafe self newItem
  = liftDOM
      (((self ^. jsf "initialize" [toJSVal newItem]) >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGPathSegList.initialize Mozilla SVGPathSegList.initialize documentation> 
initializeUnchecked ::
                    (MonadDOM m, IsSVGPathSeg newItem) =>
                      SVGPathSegList -> Maybe newItem -> m SVGPathSeg
initializeUnchecked self newItem
  = liftDOM
      ((self ^. jsf "initialize" [toJSVal newItem]) >>=
         fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGPathSegList.getItem Mozilla SVGPathSegList.getItem documentation> 
getItem ::
        (MonadDOM m) => SVGPathSegList -> Word -> m (Maybe SVGPathSeg)
getItem self index
  = liftDOM ((self ^. jsf "getItem" [toJSVal index]) >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGPathSegList.getItem Mozilla SVGPathSegList.getItem documentation> 
getItem_ :: (MonadDOM m) => SVGPathSegList -> Word -> m ()
getItem_ self index
  = liftDOM (void (self ^. jsf "getItem" [toJSVal index]))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGPathSegList.getItem Mozilla SVGPathSegList.getItem documentation> 
getItemUnsafe ::
              (MonadDOM m, HasCallStack) =>
                SVGPathSegList -> Word -> m SVGPathSeg
getItemUnsafe self index
  = liftDOM
      (((self ^. jsf "getItem" [toJSVal index]) >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGPathSegList.getItem Mozilla SVGPathSegList.getItem documentation> 
getItemUnchecked ::
                 (MonadDOM m) => SVGPathSegList -> Word -> m SVGPathSeg
getItemUnchecked self index
  = liftDOM
      ((self ^. jsf "getItem" [toJSVal index]) >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGPathSegList.insertItemBefore Mozilla SVGPathSegList.insertItemBefore documentation> 
insertItemBefore ::
                 (MonadDOM m, IsSVGPathSeg newItem) =>
                   SVGPathSegList -> Maybe newItem -> Word -> m (Maybe SVGPathSeg)
insertItemBefore self newItem index
  = liftDOM
      ((self ^. jsf "insertItemBefore" [toJSVal newItem, toJSVal index])
         >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGPathSegList.insertItemBefore Mozilla SVGPathSegList.insertItemBefore documentation> 
insertItemBefore_ ::
                  (MonadDOM m, IsSVGPathSeg newItem) =>
                    SVGPathSegList -> Maybe newItem -> Word -> m ()
insertItemBefore_ self newItem index
  = liftDOM
      (void
         (self ^. jsf "insertItemBefore" [toJSVal newItem, toJSVal index]))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGPathSegList.insertItemBefore Mozilla SVGPathSegList.insertItemBefore documentation> 
insertItemBeforeUnsafe ::
                       (MonadDOM m, IsSVGPathSeg newItem, HasCallStack) =>
                         SVGPathSegList -> Maybe newItem -> Word -> m SVGPathSeg
insertItemBeforeUnsafe self newItem index
  = liftDOM
      (((self ^. jsf "insertItemBefore" [toJSVal newItem, toJSVal index])
          >>= fromJSVal)
         >>= maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGPathSegList.insertItemBefore Mozilla SVGPathSegList.insertItemBefore documentation> 
insertItemBeforeUnchecked ::
                          (MonadDOM m, IsSVGPathSeg newItem) =>
                            SVGPathSegList -> Maybe newItem -> Word -> m SVGPathSeg
insertItemBeforeUnchecked self newItem index
  = liftDOM
      ((self ^. jsf "insertItemBefore" [toJSVal newItem, toJSVal index])
         >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGPathSegList.replaceItem Mozilla SVGPathSegList.replaceItem documentation> 
replaceItem ::
            (MonadDOM m, IsSVGPathSeg newItem) =>
              SVGPathSegList -> Maybe newItem -> Word -> m (Maybe SVGPathSeg)
replaceItem self newItem index
  = liftDOM
      ((self ^. jsf "replaceItem" [toJSVal newItem, toJSVal index]) >>=
         fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGPathSegList.replaceItem Mozilla SVGPathSegList.replaceItem documentation> 
replaceItem_ ::
             (MonadDOM m, IsSVGPathSeg newItem) =>
               SVGPathSegList -> Maybe newItem -> Word -> m ()
replaceItem_ self newItem index
  = liftDOM
      (void (self ^. jsf "replaceItem" [toJSVal newItem, toJSVal index]))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGPathSegList.replaceItem Mozilla SVGPathSegList.replaceItem documentation> 
replaceItemUnsafe ::
                  (MonadDOM m, IsSVGPathSeg newItem, HasCallStack) =>
                    SVGPathSegList -> Maybe newItem -> Word -> m SVGPathSeg
replaceItemUnsafe self newItem index
  = liftDOM
      (((self ^. jsf "replaceItem" [toJSVal newItem, toJSVal index]) >>=
          fromJSVal)
         >>= maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGPathSegList.replaceItem Mozilla SVGPathSegList.replaceItem documentation> 
replaceItemUnchecked ::
                     (MonadDOM m, IsSVGPathSeg newItem) =>
                       SVGPathSegList -> Maybe newItem -> Word -> m SVGPathSeg
replaceItemUnchecked self newItem index
  = liftDOM
      ((self ^. jsf "replaceItem" [toJSVal newItem, toJSVal index]) >>=
         fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGPathSegList.removeItem Mozilla SVGPathSegList.removeItem documentation> 
removeItem ::
           (MonadDOM m) => SVGPathSegList -> Word -> m (Maybe SVGPathSeg)
removeItem self index
  = liftDOM
      ((self ^. jsf "removeItem" [toJSVal index]) >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGPathSegList.removeItem Mozilla SVGPathSegList.removeItem documentation> 
removeItem_ :: (MonadDOM m) => SVGPathSegList -> Word -> m ()
removeItem_ self index
  = liftDOM (void (self ^. jsf "removeItem" [toJSVal index]))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGPathSegList.removeItem Mozilla SVGPathSegList.removeItem documentation> 
removeItemUnsafe ::
                 (MonadDOM m, HasCallStack) =>
                   SVGPathSegList -> Word -> m SVGPathSeg
removeItemUnsafe self index
  = liftDOM
      (((self ^. jsf "removeItem" [toJSVal index]) >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGPathSegList.removeItem Mozilla SVGPathSegList.removeItem documentation> 
removeItemUnchecked ::
                    (MonadDOM m) => SVGPathSegList -> Word -> m SVGPathSeg
removeItemUnchecked self index
  = liftDOM
      ((self ^. jsf "removeItem" [toJSVal index]) >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGPathSegList.appendItem Mozilla SVGPathSegList.appendItem documentation> 
appendItem ::
           (MonadDOM m, IsSVGPathSeg newItem) =>
             SVGPathSegList -> Maybe newItem -> m (Maybe SVGPathSeg)
appendItem self newItem
  = liftDOM
      ((self ^. jsf "appendItem" [toJSVal newItem]) >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGPathSegList.appendItem Mozilla SVGPathSegList.appendItem documentation> 
appendItem_ ::
            (MonadDOM m, IsSVGPathSeg newItem) =>
              SVGPathSegList -> Maybe newItem -> m ()
appendItem_ self newItem
  = liftDOM (void (self ^. jsf "appendItem" [toJSVal newItem]))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGPathSegList.appendItem Mozilla SVGPathSegList.appendItem documentation> 
appendItemUnsafe ::
                 (MonadDOM m, IsSVGPathSeg newItem, HasCallStack) =>
                   SVGPathSegList -> Maybe newItem -> m SVGPathSeg
appendItemUnsafe self newItem
  = liftDOM
      (((self ^. jsf "appendItem" [toJSVal newItem]) >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGPathSegList.appendItem Mozilla SVGPathSegList.appendItem documentation> 
appendItemUnchecked ::
                    (MonadDOM m, IsSVGPathSeg newItem) =>
                      SVGPathSegList -> Maybe newItem -> m SVGPathSeg
appendItemUnchecked self newItem
  = liftDOM
      ((self ^. jsf "appendItem" [toJSVal newItem]) >>=
         fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGPathSegList.numberOfItems Mozilla SVGPathSegList.numberOfItems documentation> 
getNumberOfItems :: (MonadDOM m) => SVGPathSegList -> m Word
getNumberOfItems self
  = liftDOM
      (round <$> ((self ^. js "numberOfItems") >>= valToNumber))
