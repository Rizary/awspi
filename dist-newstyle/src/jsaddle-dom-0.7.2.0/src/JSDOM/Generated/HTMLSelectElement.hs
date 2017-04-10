{-# LANGUAGE PatternSynonyms #-}
-- For HasCallStack compatibility
{-# LANGUAGE ImplicitParams, ConstraintKinds, KindSignatures #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module JSDOM.Generated.HTMLSelectElement
       (item, item_, itemUnsafe, itemUnchecked, namedItem, namedItem_,
        namedItemUnsafe, namedItemUnchecked, addBefore, add, remove,
        checkValidity, checkValidity_, setCustomValidity, setAutofocus,
        getAutofocus, setDisabled, getDisabled, getForm, getFormUnsafe,
        getFormUnchecked, setMultiple, getMultiple, setName, getName,
        setRequired, getRequired, setSize, getSize, getType, getOptions,
        getOptionsUnsafe, getOptionsUnchecked, setLength, getLength,
        getSelectedOptions, getSelectedOptionsUnsafe,
        getSelectedOptionsUnchecked, setSelectedIndex, getSelectedIndex,
        setValue, getValue, getValueUnsafe, getValueUnchecked,
        getWillValidate, getValidity, getValidityUnsafe,
        getValidityUnchecked, getValidationMessage, getLabels,
        getLabelsUnsafe, getLabelsUnchecked, HTMLSelectElement(..),
        gTypeHTMLSelectElement)
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

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLSelectElement.item Mozilla HTMLSelectElement.item documentation> 
item :: (MonadDOM m) => HTMLSelectElement -> Word -> m (Maybe Node)
item self index
  = liftDOM ((self ^. jsf "item" [toJSVal index]) >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLSelectElement.item Mozilla HTMLSelectElement.item documentation> 
item_ :: (MonadDOM m) => HTMLSelectElement -> Word -> m ()
item_ self index
  = liftDOM (void (self ^. jsf "item" [toJSVal index]))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLSelectElement.item Mozilla HTMLSelectElement.item documentation> 
itemUnsafe ::
           (MonadDOM m, HasCallStack) => HTMLSelectElement -> Word -> m Node
itemUnsafe self index
  = liftDOM
      (((self ^. jsf "item" [toJSVal index]) >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLSelectElement.item Mozilla HTMLSelectElement.item documentation> 
itemUnchecked ::
              (MonadDOM m) => HTMLSelectElement -> Word -> m Node
itemUnchecked self index
  = liftDOM
      ((self ^. jsf "item" [toJSVal index]) >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLSelectElement.namedItem Mozilla HTMLSelectElement.namedItem documentation> 
namedItem ::
          (MonadDOM m, ToJSString name) =>
            HTMLSelectElement -> name -> m (Maybe Node)
namedItem self name
  = liftDOM ((self ^. jsf "namedItem" [toJSVal name]) >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLSelectElement.namedItem Mozilla HTMLSelectElement.namedItem documentation> 
namedItem_ ::
           (MonadDOM m, ToJSString name) => HTMLSelectElement -> name -> m ()
namedItem_ self name
  = liftDOM (void (self ^. jsf "namedItem" [toJSVal name]))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLSelectElement.namedItem Mozilla HTMLSelectElement.namedItem documentation> 
namedItemUnsafe ::
                (MonadDOM m, ToJSString name, HasCallStack) =>
                  HTMLSelectElement -> name -> m Node
namedItemUnsafe self name
  = liftDOM
      (((self ^. jsf "namedItem" [toJSVal name]) >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLSelectElement.namedItem Mozilla HTMLSelectElement.namedItem documentation> 
namedItemUnchecked ::
                   (MonadDOM m, ToJSString name) =>
                     HTMLSelectElement -> name -> m Node
namedItemUnchecked self name
  = liftDOM
      ((self ^. jsf "namedItem" [toJSVal name]) >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLSelectElement.add Mozilla HTMLSelectElement.add documentation> 
addBefore ::
          (MonadDOM m, IsHTMLElement element, IsHTMLElement before) =>
            HTMLSelectElement -> Maybe element -> Maybe before -> m ()
addBefore self element before
  = liftDOM
      (void (self ^. jsf "add" [toJSVal element, toJSVal before]))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLSelectElement.add Mozilla HTMLSelectElement.add documentation> 
add ::
    (MonadDOM m, IsHTMLElement element) =>
      HTMLSelectElement -> Maybe element -> Int -> m ()
add self element index
  = liftDOM
      (void (self ^. jsf "add" [toJSVal element, toJSVal index]))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLSelectElement.remove Mozilla HTMLSelectElement.remove documentation> 
remove :: (MonadDOM m) => HTMLSelectElement -> m ()
remove self = liftDOM (void (self ^. jsf "remove" ()))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLSelectElement.checkValidity Mozilla HTMLSelectElement.checkValidity documentation> 
checkValidity :: (MonadDOM m) => HTMLSelectElement -> m Bool
checkValidity self
  = liftDOM ((self ^. jsf "checkValidity" ()) >>= valToBool)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLSelectElement.checkValidity Mozilla HTMLSelectElement.checkValidity documentation> 
checkValidity_ :: (MonadDOM m) => HTMLSelectElement -> m ()
checkValidity_ self
  = liftDOM (void (self ^. jsf "checkValidity" ()))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLSelectElement.setCustomValidity Mozilla HTMLSelectElement.setCustomValidity documentation> 
setCustomValidity ::
                  (MonadDOM m, ToJSString error) =>
                    HTMLSelectElement -> Maybe error -> m ()
setCustomValidity self error
  = liftDOM (void (self ^. jsf "setCustomValidity" [toJSVal error]))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLSelectElement.autofocus Mozilla HTMLSelectElement.autofocus documentation> 
setAutofocus :: (MonadDOM m) => HTMLSelectElement -> Bool -> m ()
setAutofocus self val
  = liftDOM (self ^. jss "autofocus" (toJSVal val))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLSelectElement.autofocus Mozilla HTMLSelectElement.autofocus documentation> 
getAutofocus :: (MonadDOM m) => HTMLSelectElement -> m Bool
getAutofocus self
  = liftDOM ((self ^. js "autofocus") >>= valToBool)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLSelectElement.disabled Mozilla HTMLSelectElement.disabled documentation> 
setDisabled :: (MonadDOM m) => HTMLSelectElement -> Bool -> m ()
setDisabled self val
  = liftDOM (self ^. jss "disabled" (toJSVal val))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLSelectElement.disabled Mozilla HTMLSelectElement.disabled documentation> 
getDisabled :: (MonadDOM m) => HTMLSelectElement -> m Bool
getDisabled self = liftDOM ((self ^. js "disabled") >>= valToBool)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLSelectElement.form Mozilla HTMLSelectElement.form documentation> 
getForm ::
        (MonadDOM m) => HTMLSelectElement -> m (Maybe HTMLFormElement)
getForm self = liftDOM ((self ^. js "form") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLSelectElement.form Mozilla HTMLSelectElement.form documentation> 
getFormUnsafe ::
              (MonadDOM m, HasCallStack) =>
                HTMLSelectElement -> m HTMLFormElement
getFormUnsafe self
  = liftDOM
      (((self ^. js "form") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLSelectElement.form Mozilla HTMLSelectElement.form documentation> 
getFormUnchecked ::
                 (MonadDOM m) => HTMLSelectElement -> m HTMLFormElement
getFormUnchecked self
  = liftDOM ((self ^. js "form") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLSelectElement.multiple Mozilla HTMLSelectElement.multiple documentation> 
setMultiple :: (MonadDOM m) => HTMLSelectElement -> Bool -> m ()
setMultiple self val
  = liftDOM (self ^. jss "multiple" (toJSVal val))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLSelectElement.multiple Mozilla HTMLSelectElement.multiple documentation> 
getMultiple :: (MonadDOM m) => HTMLSelectElement -> m Bool
getMultiple self = liftDOM ((self ^. js "multiple") >>= valToBool)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLSelectElement.name Mozilla HTMLSelectElement.name documentation> 
setName ::
        (MonadDOM m, ToJSString val) => HTMLSelectElement -> val -> m ()
setName self val = liftDOM (self ^. jss "name" (toJSVal val))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLSelectElement.name Mozilla HTMLSelectElement.name documentation> 
getName ::
        (MonadDOM m, FromJSString result) => HTMLSelectElement -> m result
getName self = liftDOM ((self ^. js "name") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLSelectElement.required Mozilla HTMLSelectElement.required documentation> 
setRequired :: (MonadDOM m) => HTMLSelectElement -> Bool -> m ()
setRequired self val
  = liftDOM (self ^. jss "required" (toJSVal val))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLSelectElement.required Mozilla HTMLSelectElement.required documentation> 
getRequired :: (MonadDOM m) => HTMLSelectElement -> m Bool
getRequired self = liftDOM ((self ^. js "required") >>= valToBool)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLSelectElement.size Mozilla HTMLSelectElement.size documentation> 
setSize :: (MonadDOM m) => HTMLSelectElement -> Int -> m ()
setSize self val = liftDOM (self ^. jss "size" (toJSVal val))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLSelectElement.size Mozilla HTMLSelectElement.size documentation> 
getSize :: (MonadDOM m) => HTMLSelectElement -> m Int
getSize self
  = liftDOM (round <$> ((self ^. js "size") >>= valToNumber))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLSelectElement.type Mozilla HTMLSelectElement.type documentation> 
getType ::
        (MonadDOM m, FromJSString result) => HTMLSelectElement -> m result
getType self = liftDOM ((self ^. js "type") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLSelectElement.options Mozilla HTMLSelectElement.options documentation> 
getOptions ::
           (MonadDOM m) =>
             HTMLSelectElement -> m (Maybe HTMLOptionsCollection)
getOptions self = liftDOM ((self ^. js "options") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLSelectElement.options Mozilla HTMLSelectElement.options documentation> 
getOptionsUnsafe ::
                 (MonadDOM m, HasCallStack) =>
                   HTMLSelectElement -> m HTMLOptionsCollection
getOptionsUnsafe self
  = liftDOM
      (((self ^. js "options") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLSelectElement.options Mozilla HTMLSelectElement.options documentation> 
getOptionsUnchecked ::
                    (MonadDOM m) => HTMLSelectElement -> m HTMLOptionsCollection
getOptionsUnchecked self
  = liftDOM ((self ^. js "options") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLSelectElement.length Mozilla HTMLSelectElement.length documentation> 
setLength :: (MonadDOM m) => HTMLSelectElement -> Word -> m ()
setLength self val = liftDOM (self ^. jss "length" (toJSVal val))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLSelectElement.length Mozilla HTMLSelectElement.length documentation> 
getLength :: (MonadDOM m) => HTMLSelectElement -> m Word
getLength self
  = liftDOM (round <$> ((self ^. js "length") >>= valToNumber))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLSelectElement.selectedOptions Mozilla HTMLSelectElement.selectedOptions documentation> 
getSelectedOptions ::
                   (MonadDOM m) => HTMLSelectElement -> m (Maybe HTMLCollection)
getSelectedOptions self
  = liftDOM ((self ^. js "selectedOptions") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLSelectElement.selectedOptions Mozilla HTMLSelectElement.selectedOptions documentation> 
getSelectedOptionsUnsafe ::
                         (MonadDOM m, HasCallStack) => HTMLSelectElement -> m HTMLCollection
getSelectedOptionsUnsafe self
  = liftDOM
      (((self ^. js "selectedOptions") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLSelectElement.selectedOptions Mozilla HTMLSelectElement.selectedOptions documentation> 
getSelectedOptionsUnchecked ::
                            (MonadDOM m) => HTMLSelectElement -> m HTMLCollection
getSelectedOptionsUnchecked self
  = liftDOM ((self ^. js "selectedOptions") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLSelectElement.selectedIndex Mozilla HTMLSelectElement.selectedIndex documentation> 
setSelectedIndex ::
                 (MonadDOM m) => HTMLSelectElement -> Int -> m ()
setSelectedIndex self val
  = liftDOM (self ^. jss "selectedIndex" (toJSVal val))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLSelectElement.selectedIndex Mozilla HTMLSelectElement.selectedIndex documentation> 
getSelectedIndex :: (MonadDOM m) => HTMLSelectElement -> m Int
getSelectedIndex self
  = liftDOM
      (round <$> ((self ^. js "selectedIndex") >>= valToNumber))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLSelectElement.value Mozilla HTMLSelectElement.value documentation> 
setValue ::
         (MonadDOM m, ToJSString val) =>
           HTMLSelectElement -> Maybe val -> m ()
setValue self val = liftDOM (self ^. jss "value" (toJSVal val))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLSelectElement.value Mozilla HTMLSelectElement.value documentation> 
getValue ::
         (MonadDOM m, FromJSString result) =>
           HTMLSelectElement -> m (Maybe result)
getValue self
  = liftDOM ((self ^. js "value") >>= fromMaybeJSString)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLSelectElement.value Mozilla HTMLSelectElement.value documentation> 
getValueUnsafe ::
               (MonadDOM m, HasCallStack, FromJSString result) =>
                 HTMLSelectElement -> m result
getValueUnsafe self
  = liftDOM
      (((self ^. js "value") >>= fromMaybeJSString) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLSelectElement.value Mozilla HTMLSelectElement.value documentation> 
getValueUnchecked ::
                  (MonadDOM m, FromJSString result) => HTMLSelectElement -> m result
getValueUnchecked self
  = liftDOM ((self ^. js "value") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLSelectElement.willValidate Mozilla HTMLSelectElement.willValidate documentation> 
getWillValidate :: (MonadDOM m) => HTMLSelectElement -> m Bool
getWillValidate self
  = liftDOM ((self ^. js "willValidate") >>= valToBool)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLSelectElement.validity Mozilla HTMLSelectElement.validity documentation> 
getValidity ::
            (MonadDOM m) => HTMLSelectElement -> m (Maybe ValidityState)
getValidity self = liftDOM ((self ^. js "validity") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLSelectElement.validity Mozilla HTMLSelectElement.validity documentation> 
getValidityUnsafe ::
                  (MonadDOM m, HasCallStack) => HTMLSelectElement -> m ValidityState
getValidityUnsafe self
  = liftDOM
      (((self ^. js "validity") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLSelectElement.validity Mozilla HTMLSelectElement.validity documentation> 
getValidityUnchecked ::
                     (MonadDOM m) => HTMLSelectElement -> m ValidityState
getValidityUnchecked self
  = liftDOM ((self ^. js "validity") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLSelectElement.validationMessage Mozilla HTMLSelectElement.validationMessage documentation> 
getValidationMessage ::
                     (MonadDOM m, FromJSString result) => HTMLSelectElement -> m result
getValidationMessage self
  = liftDOM ((self ^. js "validationMessage") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLSelectElement.labels Mozilla HTMLSelectElement.labels documentation> 
getLabels ::
          (MonadDOM m) => HTMLSelectElement -> m (Maybe NodeList)
getLabels self = liftDOM ((self ^. js "labels") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLSelectElement.labels Mozilla HTMLSelectElement.labels documentation> 
getLabelsUnsafe ::
                (MonadDOM m, HasCallStack) => HTMLSelectElement -> m NodeList
getLabelsUnsafe self
  = liftDOM
      (((self ^. js "labels") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLSelectElement.labels Mozilla HTMLSelectElement.labels documentation> 
getLabelsUnchecked ::
                   (MonadDOM m) => HTMLSelectElement -> m NodeList
getLabelsUnchecked self
  = liftDOM ((self ^. js "labels") >>= fromJSValUnchecked)
