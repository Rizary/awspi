{-# LANGUAGE PatternSynonyms #-}
-- For HasCallStack compatibility
{-# LANGUAGE ImplicitParams, ConstraintKinds, KindSignatures #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module JSDOM.Generated.HTMLTextAreaElement
       (checkValidity, checkValidity_, setCustomValidity, select,
        setRangeText, setRangeText4, setSelectionRange, setAutofocus,
        getAutofocus, setCols, getCols, setDirName, getDirName,
        setDisabled, getDisabled, getForm, getFormUnsafe, getFormUnchecked,
        setMaxLength, getMaxLength, setName, getName, setPlaceholder,
        getPlaceholder, setReadOnly, getReadOnly, setRequired, getRequired,
        setRows, getRows, setWrap, getWrap, getType, setDefaultValue,
        getDefaultValue, getDefaultValueUnsafe, getDefaultValueUnchecked,
        setValue, getValue, getValueUnsafe, getValueUnchecked,
        getTextLength, getWillValidate, getValidity, getValidityUnsafe,
        getValidityUnchecked, getValidationMessage, getLabels,
        getLabelsUnsafe, getLabelsUnchecked, setSelectionStart,
        getSelectionStart, setSelectionEnd, getSelectionEnd,
        setSelectionDirection, getSelectionDirection, setAutocorrect,
        getAutocorrect, setAutocapitalize, getAutocapitalize,
        getAutocapitalizeUnsafe, getAutocapitalizeUnchecked,
        HTMLTextAreaElement(..), gTypeHTMLTextAreaElement)
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

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.checkValidity Mozilla HTMLTextAreaElement.checkValidity documentation> 
checkValidity :: (MonadDOM m) => HTMLTextAreaElement -> m Bool
checkValidity self
  = liftDOM ((self ^. jsf "checkValidity" ()) >>= valToBool)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.checkValidity Mozilla HTMLTextAreaElement.checkValidity documentation> 
checkValidity_ :: (MonadDOM m) => HTMLTextAreaElement -> m ()
checkValidity_ self
  = liftDOM (void (self ^. jsf "checkValidity" ()))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.setCustomValidity Mozilla HTMLTextAreaElement.setCustomValidity documentation> 
setCustomValidity ::
                  (MonadDOM m, ToJSString error) =>
                    HTMLTextAreaElement -> Maybe error -> m ()
setCustomValidity self error
  = liftDOM (void (self ^. jsf "setCustomValidity" [toJSVal error]))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.select Mozilla HTMLTextAreaElement.select documentation> 
select :: (MonadDOM m) => HTMLTextAreaElement -> m ()
select self = liftDOM (void (self ^. jsf "select" ()))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.setRangeText Mozilla HTMLTextAreaElement.setRangeText documentation> 
setRangeText ::
             (MonadDOM m, ToJSString replacement) =>
               HTMLTextAreaElement -> replacement -> m ()
setRangeText self replacement
  = liftDOM (void (self ^. jsf "setRangeText" [toJSVal replacement]))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.setRangeText Mozilla HTMLTextAreaElement.setRangeText documentation> 
setRangeText4 ::
              (MonadDOM m, ToJSString replacement, ToJSString selectionMode) =>
                HTMLTextAreaElement ->
                  replacement -> Word -> Word -> selectionMode -> m ()
setRangeText4 self replacement start end selectionMode
  = liftDOM
      (void
         (self ^. jsf "setRangeText"
            [toJSVal replacement, toJSVal start, toJSVal end,
             toJSVal selectionMode]))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.setSelectionRange Mozilla HTMLTextAreaElement.setSelectionRange documentation> 
setSelectionRange ::
                  (MonadDOM m, ToJSString direction) =>
                    HTMLTextAreaElement -> Int -> Int -> direction -> m ()
setSelectionRange self start end direction
  = liftDOM
      (void
         (self ^. jsf "setSelectionRange"
            [toJSVal start, toJSVal end, toJSVal direction]))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.autofocus Mozilla HTMLTextAreaElement.autofocus documentation> 
setAutofocus :: (MonadDOM m) => HTMLTextAreaElement -> Bool -> m ()
setAutofocus self val
  = liftDOM (self ^. jss "autofocus" (toJSVal val))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.autofocus Mozilla HTMLTextAreaElement.autofocus documentation> 
getAutofocus :: (MonadDOM m) => HTMLTextAreaElement -> m Bool
getAutofocus self
  = liftDOM ((self ^. js "autofocus") >>= valToBool)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.cols Mozilla HTMLTextAreaElement.cols documentation> 
setCols :: (MonadDOM m) => HTMLTextAreaElement -> Int -> m ()
setCols self val = liftDOM (self ^. jss "cols" (toJSVal val))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.cols Mozilla HTMLTextAreaElement.cols documentation> 
getCols :: (MonadDOM m) => HTMLTextAreaElement -> m Int
getCols self
  = liftDOM (round <$> ((self ^. js "cols") >>= valToNumber))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.dirName Mozilla HTMLTextAreaElement.dirName documentation> 
setDirName ::
           (MonadDOM m, ToJSString val) => HTMLTextAreaElement -> val -> m ()
setDirName self val = liftDOM (self ^. jss "dirName" (toJSVal val))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.dirName Mozilla HTMLTextAreaElement.dirName documentation> 
getDirName ::
           (MonadDOM m, FromJSString result) =>
             HTMLTextAreaElement -> m result
getDirName self
  = liftDOM ((self ^. js "dirName") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.disabled Mozilla HTMLTextAreaElement.disabled documentation> 
setDisabled :: (MonadDOM m) => HTMLTextAreaElement -> Bool -> m ()
setDisabled self val
  = liftDOM (self ^. jss "disabled" (toJSVal val))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.disabled Mozilla HTMLTextAreaElement.disabled documentation> 
getDisabled :: (MonadDOM m) => HTMLTextAreaElement -> m Bool
getDisabled self = liftDOM ((self ^. js "disabled") >>= valToBool)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.form Mozilla HTMLTextAreaElement.form documentation> 
getForm ::
        (MonadDOM m) => HTMLTextAreaElement -> m (Maybe HTMLFormElement)
getForm self = liftDOM ((self ^. js "form") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.form Mozilla HTMLTextAreaElement.form documentation> 
getFormUnsafe ::
              (MonadDOM m, HasCallStack) =>
                HTMLTextAreaElement -> m HTMLFormElement
getFormUnsafe self
  = liftDOM
      (((self ^. js "form") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.form Mozilla HTMLTextAreaElement.form documentation> 
getFormUnchecked ::
                 (MonadDOM m) => HTMLTextAreaElement -> m HTMLFormElement
getFormUnchecked self
  = liftDOM ((self ^. js "form") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.maxLength Mozilla HTMLTextAreaElement.maxLength documentation> 
setMaxLength :: (MonadDOM m) => HTMLTextAreaElement -> Int -> m ()
setMaxLength self val
  = liftDOM (self ^. jss "maxLength" (toJSVal val))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.maxLength Mozilla HTMLTextAreaElement.maxLength documentation> 
getMaxLength :: (MonadDOM m) => HTMLTextAreaElement -> m Int
getMaxLength self
  = liftDOM (round <$> ((self ^. js "maxLength") >>= valToNumber))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.name Mozilla HTMLTextAreaElement.name documentation> 
setName ::
        (MonadDOM m, ToJSString val) => HTMLTextAreaElement -> val -> m ()
setName self val = liftDOM (self ^. jss "name" (toJSVal val))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.name Mozilla HTMLTextAreaElement.name documentation> 
getName ::
        (MonadDOM m, FromJSString result) =>
          HTMLTextAreaElement -> m result
getName self = liftDOM ((self ^. js "name") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.placeholder Mozilla HTMLTextAreaElement.placeholder documentation> 
setPlaceholder ::
               (MonadDOM m, ToJSString val) => HTMLTextAreaElement -> val -> m ()
setPlaceholder self val
  = liftDOM (self ^. jss "placeholder" (toJSVal val))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.placeholder Mozilla HTMLTextAreaElement.placeholder documentation> 
getPlaceholder ::
               (MonadDOM m, FromJSString result) =>
                 HTMLTextAreaElement -> m result
getPlaceholder self
  = liftDOM ((self ^. js "placeholder") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.readOnly Mozilla HTMLTextAreaElement.readOnly documentation> 
setReadOnly :: (MonadDOM m) => HTMLTextAreaElement -> Bool -> m ()
setReadOnly self val
  = liftDOM (self ^. jss "readOnly" (toJSVal val))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.readOnly Mozilla HTMLTextAreaElement.readOnly documentation> 
getReadOnly :: (MonadDOM m) => HTMLTextAreaElement -> m Bool
getReadOnly self = liftDOM ((self ^. js "readOnly") >>= valToBool)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.required Mozilla HTMLTextAreaElement.required documentation> 
setRequired :: (MonadDOM m) => HTMLTextAreaElement -> Bool -> m ()
setRequired self val
  = liftDOM (self ^. jss "required" (toJSVal val))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.required Mozilla HTMLTextAreaElement.required documentation> 
getRequired :: (MonadDOM m) => HTMLTextAreaElement -> m Bool
getRequired self = liftDOM ((self ^. js "required") >>= valToBool)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.rows Mozilla HTMLTextAreaElement.rows documentation> 
setRows :: (MonadDOM m) => HTMLTextAreaElement -> Int -> m ()
setRows self val = liftDOM (self ^. jss "rows" (toJSVal val))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.rows Mozilla HTMLTextAreaElement.rows documentation> 
getRows :: (MonadDOM m) => HTMLTextAreaElement -> m Int
getRows self
  = liftDOM (round <$> ((self ^. js "rows") >>= valToNumber))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.wrap Mozilla HTMLTextAreaElement.wrap documentation> 
setWrap ::
        (MonadDOM m, ToJSString val) => HTMLTextAreaElement -> val -> m ()
setWrap self val = liftDOM (self ^. jss "wrap" (toJSVal val))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.wrap Mozilla HTMLTextAreaElement.wrap documentation> 
getWrap ::
        (MonadDOM m, FromJSString result) =>
          HTMLTextAreaElement -> m result
getWrap self = liftDOM ((self ^. js "wrap") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.type Mozilla HTMLTextAreaElement.type documentation> 
getType ::
        (MonadDOM m, FromJSString result) =>
          HTMLTextAreaElement -> m result
getType self = liftDOM ((self ^. js "type") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.defaultValue Mozilla HTMLTextAreaElement.defaultValue documentation> 
setDefaultValue ::
                (MonadDOM m, ToJSString val) =>
                  HTMLTextAreaElement -> Maybe val -> m ()
setDefaultValue self val
  = liftDOM (self ^. jss "defaultValue" (toJSVal val))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.defaultValue Mozilla HTMLTextAreaElement.defaultValue documentation> 
getDefaultValue ::
                (MonadDOM m, FromJSString result) =>
                  HTMLTextAreaElement -> m (Maybe result)
getDefaultValue self
  = liftDOM ((self ^. js "defaultValue") >>= fromMaybeJSString)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.defaultValue Mozilla HTMLTextAreaElement.defaultValue documentation> 
getDefaultValueUnsafe ::
                      (MonadDOM m, HasCallStack, FromJSString result) =>
                        HTMLTextAreaElement -> m result
getDefaultValueUnsafe self
  = liftDOM
      (((self ^. js "defaultValue") >>= fromMaybeJSString) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.defaultValue Mozilla HTMLTextAreaElement.defaultValue documentation> 
getDefaultValueUnchecked ::
                         (MonadDOM m, FromJSString result) =>
                           HTMLTextAreaElement -> m result
getDefaultValueUnchecked self
  = liftDOM ((self ^. js "defaultValue") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.value Mozilla HTMLTextAreaElement.value documentation> 
setValue ::
         (MonadDOM m, ToJSString val) =>
           HTMLTextAreaElement -> Maybe val -> m ()
setValue self val = liftDOM (self ^. jss "value" (toJSVal val))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.value Mozilla HTMLTextAreaElement.value documentation> 
getValue ::
         (MonadDOM m, FromJSString result) =>
           HTMLTextAreaElement -> m (Maybe result)
getValue self
  = liftDOM ((self ^. js "value") >>= fromMaybeJSString)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.value Mozilla HTMLTextAreaElement.value documentation> 
getValueUnsafe ::
               (MonadDOM m, HasCallStack, FromJSString result) =>
                 HTMLTextAreaElement -> m result
getValueUnsafe self
  = liftDOM
      (((self ^. js "value") >>= fromMaybeJSString) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.value Mozilla HTMLTextAreaElement.value documentation> 
getValueUnchecked ::
                  (MonadDOM m, FromJSString result) =>
                    HTMLTextAreaElement -> m result
getValueUnchecked self
  = liftDOM ((self ^. js "value") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.textLength Mozilla HTMLTextAreaElement.textLength documentation> 
getTextLength :: (MonadDOM m) => HTMLTextAreaElement -> m Word
getTextLength self
  = liftDOM (round <$> ((self ^. js "textLength") >>= valToNumber))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.willValidate Mozilla HTMLTextAreaElement.willValidate documentation> 
getWillValidate :: (MonadDOM m) => HTMLTextAreaElement -> m Bool
getWillValidate self
  = liftDOM ((self ^. js "willValidate") >>= valToBool)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.validity Mozilla HTMLTextAreaElement.validity documentation> 
getValidity ::
            (MonadDOM m) => HTMLTextAreaElement -> m (Maybe ValidityState)
getValidity self = liftDOM ((self ^. js "validity") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.validity Mozilla HTMLTextAreaElement.validity documentation> 
getValidityUnsafe ::
                  (MonadDOM m, HasCallStack) =>
                    HTMLTextAreaElement -> m ValidityState
getValidityUnsafe self
  = liftDOM
      (((self ^. js "validity") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.validity Mozilla HTMLTextAreaElement.validity documentation> 
getValidityUnchecked ::
                     (MonadDOM m) => HTMLTextAreaElement -> m ValidityState
getValidityUnchecked self
  = liftDOM ((self ^. js "validity") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.validationMessage Mozilla HTMLTextAreaElement.validationMessage documentation> 
getValidationMessage ::
                     (MonadDOM m, FromJSString result) =>
                       HTMLTextAreaElement -> m result
getValidationMessage self
  = liftDOM ((self ^. js "validationMessage") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.labels Mozilla HTMLTextAreaElement.labels documentation> 
getLabels ::
          (MonadDOM m) => HTMLTextAreaElement -> m (Maybe NodeList)
getLabels self = liftDOM ((self ^. js "labels") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.labels Mozilla HTMLTextAreaElement.labels documentation> 
getLabelsUnsafe ::
                (MonadDOM m, HasCallStack) => HTMLTextAreaElement -> m NodeList
getLabelsUnsafe self
  = liftDOM
      (((self ^. js "labels") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.labels Mozilla HTMLTextAreaElement.labels documentation> 
getLabelsUnchecked ::
                   (MonadDOM m) => HTMLTextAreaElement -> m NodeList
getLabelsUnchecked self
  = liftDOM ((self ^. js "labels") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.selectionStart Mozilla HTMLTextAreaElement.selectionStart documentation> 
setSelectionStart ::
                  (MonadDOM m) => HTMLTextAreaElement -> Int -> m ()
setSelectionStart self val
  = liftDOM (self ^. jss "selectionStart" (toJSVal val))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.selectionStart Mozilla HTMLTextAreaElement.selectionStart documentation> 
getSelectionStart :: (MonadDOM m) => HTMLTextAreaElement -> m Int
getSelectionStart self
  = liftDOM
      (round <$> ((self ^. js "selectionStart") >>= valToNumber))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.selectionEnd Mozilla HTMLTextAreaElement.selectionEnd documentation> 
setSelectionEnd ::
                (MonadDOM m) => HTMLTextAreaElement -> Int -> m ()
setSelectionEnd self val
  = liftDOM (self ^. jss "selectionEnd" (toJSVal val))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.selectionEnd Mozilla HTMLTextAreaElement.selectionEnd documentation> 
getSelectionEnd :: (MonadDOM m) => HTMLTextAreaElement -> m Int
getSelectionEnd self
  = liftDOM (round <$> ((self ^. js "selectionEnd") >>= valToNumber))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.selectionDirection Mozilla HTMLTextAreaElement.selectionDirection documentation> 
setSelectionDirection ::
                      (MonadDOM m, ToJSString val) => HTMLTextAreaElement -> val -> m ()
setSelectionDirection self val
  = liftDOM (self ^. jss "selectionDirection" (toJSVal val))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.selectionDirection Mozilla HTMLTextAreaElement.selectionDirection documentation> 
getSelectionDirection ::
                      (MonadDOM m, FromJSString result) =>
                        HTMLTextAreaElement -> m result
getSelectionDirection self
  = liftDOM
      ((self ^. js "selectionDirection") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.autocorrect Mozilla HTMLTextAreaElement.autocorrect documentation> 
setAutocorrect ::
               (MonadDOM m) => HTMLTextAreaElement -> Bool -> m ()
setAutocorrect self val
  = liftDOM (self ^. jss "autocorrect" (toJSVal val))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.autocorrect Mozilla HTMLTextAreaElement.autocorrect documentation> 
getAutocorrect :: (MonadDOM m) => HTMLTextAreaElement -> m Bool
getAutocorrect self
  = liftDOM ((self ^. js "autocorrect") >>= valToBool)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.autocapitalize Mozilla HTMLTextAreaElement.autocapitalize documentation> 
setAutocapitalize ::
                  (MonadDOM m, ToJSString val) =>
                    HTMLTextAreaElement -> Maybe val -> m ()
setAutocapitalize self val
  = liftDOM (self ^. jss "autocapitalize" (toJSVal val))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.autocapitalize Mozilla HTMLTextAreaElement.autocapitalize documentation> 
getAutocapitalize ::
                  (MonadDOM m, FromJSString result) =>
                    HTMLTextAreaElement -> m (Maybe result)
getAutocapitalize self
  = liftDOM ((self ^. js "autocapitalize") >>= fromMaybeJSString)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.autocapitalize Mozilla HTMLTextAreaElement.autocapitalize documentation> 
getAutocapitalizeUnsafe ::
                        (MonadDOM m, HasCallStack, FromJSString result) =>
                          HTMLTextAreaElement -> m result
getAutocapitalizeUnsafe self
  = liftDOM
      (((self ^. js "autocapitalize") >>= fromMaybeJSString) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement.autocapitalize Mozilla HTMLTextAreaElement.autocapitalize documentation> 
getAutocapitalizeUnchecked ::
                           (MonadDOM m, FromJSString result) =>
                             HTMLTextAreaElement -> m result
getAutocapitalizeUnchecked self
  = liftDOM ((self ^. js "autocapitalize") >>= fromJSValUnchecked)
