{-# LANGUAGE PatternSynonyms #-}
-- For HasCallStack compatibility
{-# LANGUAGE ImplicitParams, ConstraintKinds, KindSignatures #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module JSDOM.Generated.Window
       (openDatabase, openDatabase_, openDatabaseUnsafe,
        openDatabaseUnchecked, getSelection, getSelection_,
        getSelectionUnsafe, getSelectionUnchecked, focus, blur, close,
        print, stop, open, open_, openUnsafe, openUnchecked,
        showModalDialog, showModalDialog_, alert, confirm, confirm_,
        prompt, prompt_, promptUnsafe, promptUnchecked, find, find_,
        scrollBy, scrollTo, scroll, moveBy, moveTo, resizeBy, resizeTo,
        matchMedia, matchMedia_, matchMediaUnsafe, matchMediaUnchecked,
        getComputedStyle, getComputedStyle_, getComputedStyleUnsafe,
        getComputedStyleUnchecked, getMatchedCSSRules, getMatchedCSSRules_,
        getMatchedCSSRulesUnsafe, getMatchedCSSRulesUnchecked,
        webkitConvertPointFromPageToNode,
        webkitConvertPointFromPageToNode_,
        webkitConvertPointFromPageToNodeUnsafe,
        webkitConvertPointFromPageToNodeUnchecked,
        webkitConvertPointFromNodeToPage,
        webkitConvertPointFromNodeToPage_,
        webkitConvertPointFromNodeToPageUnsafe,
        webkitConvertPointFromNodeToPageUnchecked, postMessage,
        requestAnimationFrame, requestAnimationFrame_,
        cancelAnimationFrame, webkitRequestAnimationFrame,
        webkitRequestAnimationFrame_, webkitCancelAnimationFrame,
        webkitCancelRequestAnimationFrame, captureEvents, releaseEvents,
        getWebkitIndexedDB, getWebkitIndexedDBUnsafe,
        getWebkitIndexedDBUnchecked, getIndexedDB, getIndexedDBUnsafe,
        getIndexedDBUnchecked, getWebkitStorageInfo,
        getWebkitStorageInfoUnsafe, getWebkitStorageInfoUnchecked,
        getSpeechSynthesis, getSpeechSynthesisUnsafe,
        getSpeechSynthesisUnchecked, getScreen, getScreenUnsafe,
        getScreenUnchecked, getHistory, getHistoryUnsafe,
        getHistoryUnchecked, getLocationbar, getLocationbarUnsafe,
        getLocationbarUnchecked, getMenubar, getMenubarUnsafe,
        getMenubarUnchecked, getPersonalbar, getPersonalbarUnsafe,
        getPersonalbarUnchecked, getScrollbars, getScrollbarsUnsafe,
        getScrollbarsUnchecked, getStatusbar, getStatusbarUnsafe,
        getStatusbarUnchecked, getToolbar, getToolbarUnsafe,
        getToolbarUnchecked, getNavigator, getNavigatorUnsafe,
        getNavigatorUnchecked, getClientInformation,
        getClientInformationUnsafe, getClientInformationUnchecked,
        getCrypto, getCryptoUnsafe, getCryptoUnchecked, setLocation,
        getLocation, getLocationUnsafe, getLocationUnchecked, getEvent,
        getEventUnsafe, getEventUnchecked, getFrameElement,
        getFrameElementUnsafe, getFrameElementUnchecked,
        getOffscreenBuffering, getOuterHeight, getOuterWidth,
        getInnerHeight, getInnerWidth, getScreenX, getScreenY,
        getScreenLeft, getScreenTop, getScrollX, getScrollY,
        getPageXOffset, getPageYOffset, getClosed, getLength, setName,
        getName, setStatus, getStatus, setDefaultStatus, getDefaultStatus,
        setDefaultstatus, getDefaultstatus, getSelf, getSelfUnsafe,
        getSelfUnchecked, getWindow, getWindowUnsafe, getWindowUnchecked,
        getFrames, getFramesUnsafe, getFramesUnchecked, getOpener,
        getOpenerUnsafe, getOpenerUnchecked, getParent, getParentUnsafe,
        getParentUnchecked, getTop, getTopUnsafe, getTopUnchecked,
        getDocument, getDocumentUnsafe, getDocumentUnchecked,
        getStyleMedia, getStyleMediaUnsafe, getStyleMediaUnchecked,
        getDevicePixelRatio, getApplicationCache,
        getApplicationCacheUnsafe, getApplicationCacheUnchecked,
        getSessionStorage, getSessionStorageUnsafe,
        getSessionStorageUnchecked, getLocalStorage, getLocalStorageUnsafe,
        getLocalStorageUnchecked, getOrientation, getPerformance,
        getPerformanceUnsafe, getPerformanceUnchecked, getCSS,
        getCSSUnsafe, getCSSUnchecked, abort, beforeUnload, blurEvent,
        canPlay, canPlayThrough, change, click, contextMenu, dblClick,
        drag, dragEnd, dragEnter, dragLeave, dragOver, dragStart, drop,
        durationChange, emptied, ended, error, focusEvent, hashChange,
        input, invalid, keyDown, keyPress, keyUp, load, loadedData,
        loadedMetadata, loadStart, message, mouseDown, mouseEnter,
        mouseLeave, mouseMove, mouseOut, mouseOver, mouseUp, mouseWheel,
        offline, online, pageHide, pageShow, pause, play, playing,
        popState, progress, rateChange, resize, scrollEvent, seeked,
        seeking, select, stalled, storage, submit, suspend, timeUpdate,
        unload, volumeChange, waiting, wheel, reset, search,
        webKitAnimationEnd, webKitAnimationIteration, webKitAnimationStart,
        animationEnd, animationIteration, animationStart,
        webKitTransitionEnd, transitionEnd, orientationChange, touchStart,
        touchMove, touchEnd, touchCancel, gestureStart, gestureChange,
        gestureEnd, deviceMotion, deviceOrientation, webKitDeviceProximity,
        webKitWillRevealBottom, webKitWillRevealLeft,
        webKitWillRevealRight, webKitWillRevealTop, Window(..),
        gTypeWindow)
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

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.openDatabase Mozilla Window.openDatabase documentation> 
openDatabase ::
             (MonadDOM m, ToJSString name, ToJSString version,
              ToJSString displayName) =>
               Window ->
                 name ->
                   version ->
                     displayName -> Word -> Maybe DatabaseCallback -> m (Maybe Database)
openDatabase self name version displayName estimatedSize
  creationCallback
  = liftDOM
      ((self ^. jsf "openDatabase"
          [toJSVal name, toJSVal version, toJSVal displayName,
           toJSVal estimatedSize, toJSVal creationCallback])
         >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.openDatabase Mozilla Window.openDatabase documentation> 
openDatabase_ ::
              (MonadDOM m, ToJSString name, ToJSString version,
               ToJSString displayName) =>
                Window ->
                  name ->
                    version -> displayName -> Word -> Maybe DatabaseCallback -> m ()
openDatabase_ self name version displayName estimatedSize
  creationCallback
  = liftDOM
      (void
         (self ^. jsf "openDatabase"
            [toJSVal name, toJSVal version, toJSVal displayName,
             toJSVal estimatedSize, toJSVal creationCallback]))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.openDatabase Mozilla Window.openDatabase documentation> 
openDatabaseUnsafe ::
                   (MonadDOM m, ToJSString name, ToJSString version,
                    ToJSString displayName, HasCallStack) =>
                     Window ->
                       name ->
                         version ->
                           displayName -> Word -> Maybe DatabaseCallback -> m Database
openDatabaseUnsafe self name version displayName estimatedSize
  creationCallback
  = liftDOM
      (((self ^. jsf "openDatabase"
           [toJSVal name, toJSVal version, toJSVal displayName,
            toJSVal estimatedSize, toJSVal creationCallback])
          >>= fromJSVal)
         >>= maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.openDatabase Mozilla Window.openDatabase documentation> 
openDatabaseUnchecked ::
                      (MonadDOM m, ToJSString name, ToJSString version,
                       ToJSString displayName) =>
                        Window ->
                          name ->
                            version ->
                              displayName -> Word -> Maybe DatabaseCallback -> m Database
openDatabaseUnchecked self name version displayName estimatedSize
  creationCallback
  = liftDOM
      ((self ^. jsf "openDatabase"
          [toJSVal name, toJSVal version, toJSVal displayName,
           toJSVal estimatedSize, toJSVal creationCallback])
         >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.getSelection Mozilla Window.getSelection documentation> 
getSelection :: (MonadDOM m) => Window -> m (Maybe Selection)
getSelection self
  = liftDOM ((self ^. jsf "getSelection" ()) >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.getSelection Mozilla Window.getSelection documentation> 
getSelection_ :: (MonadDOM m) => Window -> m ()
getSelection_ self = liftDOM (void (self ^. jsf "getSelection" ()))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.getSelection Mozilla Window.getSelection documentation> 
getSelectionUnsafe ::
                   (MonadDOM m, HasCallStack) => Window -> m Selection
getSelectionUnsafe self
  = liftDOM
      (((self ^. jsf "getSelection" ()) >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.getSelection Mozilla Window.getSelection documentation> 
getSelectionUnchecked :: (MonadDOM m) => Window -> m Selection
getSelectionUnchecked self
  = liftDOM ((self ^. jsf "getSelection" ()) >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.focus Mozilla Window.focus documentation> 
focus :: (MonadDOM m) => Window -> m ()
focus self = liftDOM (void (self ^. jsf "focus" ()))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.blur Mozilla Window.blur documentation> 
blur :: (MonadDOM m) => Window -> m ()
blur self = liftDOM (void (self ^. jsf "blur" ()))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.close Mozilla Window.close documentation> 
close :: (MonadDOM m) => Window -> m ()
close self = liftDOM (void (self ^. jsf "close" ()))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.print Mozilla Window.print documentation> 
print :: (MonadDOM m) => Window -> m ()
print self = liftDOM (void (self ^. jsf "print" ()))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.stop Mozilla Window.stop documentation> 
stop :: (MonadDOM m) => Window -> m ()
stop self = liftDOM (void (self ^. jsf "stop" ()))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.open Mozilla Window.open documentation> 
open ::
     (MonadDOM m, ToJSString url, ToJSString name,
      ToJSString options) =>
       Window -> url -> name -> options -> m (Maybe Window)
open self url name options
  = liftDOM
      ((self ^. jsf "open" [toJSVal url, toJSVal name, toJSVal options])
         >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.open Mozilla Window.open documentation> 
open_ ::
      (MonadDOM m, ToJSString url, ToJSString name,
       ToJSString options) =>
        Window -> url -> name -> options -> m ()
open_ self url name options
  = liftDOM
      (void
         (self ^. jsf "open" [toJSVal url, toJSVal name, toJSVal options]))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.open Mozilla Window.open documentation> 
openUnsafe ::
           (MonadDOM m, ToJSString url, ToJSString name, ToJSString options,
            HasCallStack) =>
             Window -> url -> name -> options -> m Window
openUnsafe self url name options
  = liftDOM
      (((self ^. jsf "open" [toJSVal url, toJSVal name, toJSVal options])
          >>= fromJSVal)
         >>= maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.open Mozilla Window.open documentation> 
openUnchecked ::
              (MonadDOM m, ToJSString url, ToJSString name,
               ToJSString options) =>
                Window -> url -> name -> options -> m Window
openUnchecked self url name options
  = liftDOM
      ((self ^. jsf "open" [toJSVal url, toJSVal name, toJSVal options])
         >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.showModalDialog Mozilla Window.showModalDialog documentation> 
showModalDialog ::
                (MonadDOM m, ToJSString url, ToJSString featureArgs) =>
                  Window -> url -> JSVal -> featureArgs -> m JSVal
showModalDialog self url dialogArgs featureArgs
  = liftDOM
      ((self ^. jsf "showModalDialog"
          [toJSVal url, toJSVal dialogArgs, toJSVal featureArgs])
         >>= toJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.showModalDialog Mozilla Window.showModalDialog documentation> 
showModalDialog_ ::
                 (MonadDOM m, ToJSString url, ToJSString featureArgs) =>
                   Window -> url -> JSVal -> featureArgs -> m ()
showModalDialog_ self url dialogArgs featureArgs
  = liftDOM
      (void
         (self ^. jsf "showModalDialog"
            [toJSVal url, toJSVal dialogArgs, toJSVal featureArgs]))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.alert Mozilla Window.alert documentation> 
alert ::
      (MonadDOM m, ToJSString message) => Window -> message -> m ()
alert self message
  = liftDOM (void (self ^. jsf "alert" [toJSVal message]))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.confirm Mozilla Window.confirm documentation> 
confirm ::
        (MonadDOM m, ToJSString message) => Window -> message -> m Bool
confirm self message
  = liftDOM ((self ^. jsf "confirm" [toJSVal message]) >>= valToBool)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.confirm Mozilla Window.confirm documentation> 
confirm_ ::
         (MonadDOM m, ToJSString message) => Window -> message -> m ()
confirm_ self message
  = liftDOM (void (self ^. jsf "confirm" [toJSVal message]))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.prompt Mozilla Window.prompt documentation> 
prompt ::
       (MonadDOM m, ToJSString message, ToJSString defaultValue,
        FromJSString result) =>
         Window -> message -> Maybe defaultValue -> m (Maybe result)
prompt self message defaultValue
  = liftDOM
      ((self ^. jsf "prompt" [toJSVal message, toJSVal defaultValue]) >>=
         fromMaybeJSString)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.prompt Mozilla Window.prompt documentation> 
prompt_ ::
        (MonadDOM m, ToJSString message, ToJSString defaultValue) =>
          Window -> message -> Maybe defaultValue -> m ()
prompt_ self message defaultValue
  = liftDOM
      (void
         (self ^. jsf "prompt" [toJSVal message, toJSVal defaultValue]))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.prompt Mozilla Window.prompt documentation> 
promptUnsafe ::
             (MonadDOM m, ToJSString message, ToJSString defaultValue,
              HasCallStack, FromJSString result) =>
               Window -> message -> Maybe defaultValue -> m result
promptUnsafe self message defaultValue
  = liftDOM
      (((self ^. jsf "prompt" [toJSVal message, toJSVal defaultValue])
          >>= fromMaybeJSString)
         >>= maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.prompt Mozilla Window.prompt documentation> 
promptUnchecked ::
                (MonadDOM m, ToJSString message, ToJSString defaultValue,
                 FromJSString result) =>
                  Window -> message -> Maybe defaultValue -> m result
promptUnchecked self message defaultValue
  = liftDOM
      ((self ^. jsf "prompt" [toJSVal message, toJSVal defaultValue]) >>=
         fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.find Mozilla Window.find documentation> 
find ::
     (MonadDOM m, ToJSString string) =>
       Window ->
         string -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> m Bool
find self string caseSensitive backwards wrap wholeWord
  searchInFrames showDialog
  = liftDOM
      ((self ^. jsf "find"
          [toJSVal string, toJSVal caseSensitive, toJSVal backwards,
           toJSVal wrap, toJSVal wholeWord, toJSVal searchInFrames,
           toJSVal showDialog])
         >>= valToBool)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.find Mozilla Window.find documentation> 
find_ ::
      (MonadDOM m, ToJSString string) =>
        Window ->
          string -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> m ()
find_ self string caseSensitive backwards wrap wholeWord
  searchInFrames showDialog
  = liftDOM
      (void
         (self ^. jsf "find"
            [toJSVal string, toJSVal caseSensitive, toJSVal backwards,
             toJSVal wrap, toJSVal wholeWord, toJSVal searchInFrames,
             toJSVal showDialog]))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.scrollBy Mozilla Window.scrollBy documentation> 
scrollBy :: (MonadDOM m) => Window -> Int -> Int -> m ()
scrollBy self x y
  = liftDOM (void (self ^. jsf "scrollBy" [toJSVal x, toJSVal y]))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.scrollTo Mozilla Window.scrollTo documentation> 
scrollTo :: (MonadDOM m) => Window -> Int -> Int -> m ()
scrollTo self x y
  = liftDOM (void (self ^. jsf "scrollTo" [toJSVal x, toJSVal y]))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.scroll Mozilla Window.scroll documentation> 
scroll :: (MonadDOM m) => Window -> Int -> Int -> m ()
scroll self x y
  = liftDOM (void (self ^. jsf "scroll" [toJSVal x, toJSVal y]))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.moveBy Mozilla Window.moveBy documentation> 
moveBy :: (MonadDOM m) => Window -> Float -> Float -> m ()
moveBy self x y
  = liftDOM (void (self ^. jsf "moveBy" [toJSVal x, toJSVal y]))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.moveTo Mozilla Window.moveTo documentation> 
moveTo :: (MonadDOM m) => Window -> Float -> Float -> m ()
moveTo self x y
  = liftDOM (void (self ^. jsf "moveTo" [toJSVal x, toJSVal y]))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.resizeBy Mozilla Window.resizeBy documentation> 
resizeBy :: (MonadDOM m) => Window -> Float -> Float -> m ()
resizeBy self x y
  = liftDOM (void (self ^. jsf "resizeBy" [toJSVal x, toJSVal y]))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.resizeTo Mozilla Window.resizeTo documentation> 
resizeTo :: (MonadDOM m) => Window -> Float -> Float -> m ()
resizeTo self width height
  = liftDOM
      (void (self ^. jsf "resizeTo" [toJSVal width, toJSVal height]))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.matchMedia Mozilla Window.matchMedia documentation> 
matchMedia ::
           (MonadDOM m, ToJSString query) =>
             Window -> query -> m (Maybe MediaQueryList)
matchMedia self query
  = liftDOM
      ((self ^. jsf "matchMedia" [toJSVal query]) >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.matchMedia Mozilla Window.matchMedia documentation> 
matchMedia_ ::
            (MonadDOM m, ToJSString query) => Window -> query -> m ()
matchMedia_ self query
  = liftDOM (void (self ^. jsf "matchMedia" [toJSVal query]))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.matchMedia Mozilla Window.matchMedia documentation> 
matchMediaUnsafe ::
                 (MonadDOM m, ToJSString query, HasCallStack) =>
                   Window -> query -> m MediaQueryList
matchMediaUnsafe self query
  = liftDOM
      (((self ^. jsf "matchMedia" [toJSVal query]) >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.matchMedia Mozilla Window.matchMedia documentation> 
matchMediaUnchecked ::
                    (MonadDOM m, ToJSString query) =>
                      Window -> query -> m MediaQueryList
matchMediaUnchecked self query
  = liftDOM
      ((self ^. jsf "matchMedia" [toJSVal query]) >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.getComputedStyle Mozilla Window.getComputedStyle documentation> 
getComputedStyle ::
                 (MonadDOM m, IsElement element, ToJSString pseudoElement) =>
                   Window ->
                     Maybe element ->
                       Maybe pseudoElement -> m (Maybe CSSStyleDeclaration)
getComputedStyle self element pseudoElement
  = liftDOM
      ((self ^. jsf "getComputedStyle"
          [toJSVal element, toJSVal pseudoElement])
         >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.getComputedStyle Mozilla Window.getComputedStyle documentation> 
getComputedStyle_ ::
                  (MonadDOM m, IsElement element, ToJSString pseudoElement) =>
                    Window -> Maybe element -> Maybe pseudoElement -> m ()
getComputedStyle_ self element pseudoElement
  = liftDOM
      (void
         (self ^. jsf "getComputedStyle"
            [toJSVal element, toJSVal pseudoElement]))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.getComputedStyle Mozilla Window.getComputedStyle documentation> 
getComputedStyleUnsafe ::
                       (MonadDOM m, IsElement element, ToJSString pseudoElement,
                        HasCallStack) =>
                         Window ->
                           Maybe element -> Maybe pseudoElement -> m CSSStyleDeclaration
getComputedStyleUnsafe self element pseudoElement
  = liftDOM
      (((self ^. jsf "getComputedStyle"
           [toJSVal element, toJSVal pseudoElement])
          >>= fromJSVal)
         >>= maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.getComputedStyle Mozilla Window.getComputedStyle documentation> 
getComputedStyleUnchecked ::
                          (MonadDOM m, IsElement element, ToJSString pseudoElement) =>
                            Window ->
                              Maybe element -> Maybe pseudoElement -> m CSSStyleDeclaration
getComputedStyleUnchecked self element pseudoElement
  = liftDOM
      ((self ^. jsf "getComputedStyle"
          [toJSVal element, toJSVal pseudoElement])
         >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.getMatchedCSSRules Mozilla Window.getMatchedCSSRules documentation> 
getMatchedCSSRules ::
                   (MonadDOM m, IsElement element, ToJSString pseudoElement) =>
                     Window ->
                       Maybe element -> Maybe pseudoElement -> m (Maybe CSSRuleList)
getMatchedCSSRules self element pseudoElement
  = liftDOM
      ((self ^. jsf "getMatchedCSSRules"
          [toJSVal element, toJSVal pseudoElement])
         >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.getMatchedCSSRules Mozilla Window.getMatchedCSSRules documentation> 
getMatchedCSSRules_ ::
                    (MonadDOM m, IsElement element, ToJSString pseudoElement) =>
                      Window -> Maybe element -> Maybe pseudoElement -> m ()
getMatchedCSSRules_ self element pseudoElement
  = liftDOM
      (void
         (self ^. jsf "getMatchedCSSRules"
            [toJSVal element, toJSVal pseudoElement]))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.getMatchedCSSRules Mozilla Window.getMatchedCSSRules documentation> 
getMatchedCSSRulesUnsafe ::
                         (MonadDOM m, IsElement element, ToJSString pseudoElement,
                          HasCallStack) =>
                           Window -> Maybe element -> Maybe pseudoElement -> m CSSRuleList
getMatchedCSSRulesUnsafe self element pseudoElement
  = liftDOM
      (((self ^. jsf "getMatchedCSSRules"
           [toJSVal element, toJSVal pseudoElement])
          >>= fromJSVal)
         >>= maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.getMatchedCSSRules Mozilla Window.getMatchedCSSRules documentation> 
getMatchedCSSRulesUnchecked ::
                            (MonadDOM m, IsElement element, ToJSString pseudoElement) =>
                              Window -> Maybe element -> Maybe pseudoElement -> m CSSRuleList
getMatchedCSSRulesUnchecked self element pseudoElement
  = liftDOM
      ((self ^. jsf "getMatchedCSSRules"
          [toJSVal element, toJSVal pseudoElement])
         >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.webkitConvertPointFromPageToNode Mozilla Window.webkitConvertPointFromPageToNode documentation> 
webkitConvertPointFromPageToNode ::
                                 (MonadDOM m, IsNode node) =>
                                   Window ->
                                     Maybe node -> Maybe WebKitPoint -> m (Maybe WebKitPoint)
webkitConvertPointFromPageToNode self node p
  = liftDOM
      ((self ^. jsf "webkitConvertPointFromPageToNode"
          [toJSVal node, toJSVal p])
         >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.webkitConvertPointFromPageToNode Mozilla Window.webkitConvertPointFromPageToNode documentation> 
webkitConvertPointFromPageToNode_ ::
                                  (MonadDOM m, IsNode node) =>
                                    Window -> Maybe node -> Maybe WebKitPoint -> m ()
webkitConvertPointFromPageToNode_ self node p
  = liftDOM
      (void
         (self ^. jsf "webkitConvertPointFromPageToNode"
            [toJSVal node, toJSVal p]))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.webkitConvertPointFromPageToNode Mozilla Window.webkitConvertPointFromPageToNode documentation> 
webkitConvertPointFromPageToNodeUnsafe ::
                                       (MonadDOM m, IsNode node, HasCallStack) =>
                                         Window -> Maybe node -> Maybe WebKitPoint -> m WebKitPoint
webkitConvertPointFromPageToNodeUnsafe self node p
  = liftDOM
      (((self ^. jsf "webkitConvertPointFromPageToNode"
           [toJSVal node, toJSVal p])
          >>= fromJSVal)
         >>= maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.webkitConvertPointFromPageToNode Mozilla Window.webkitConvertPointFromPageToNode documentation> 
webkitConvertPointFromPageToNodeUnchecked ::
                                          (MonadDOM m, IsNode node) =>
                                            Window ->
                                              Maybe node -> Maybe WebKitPoint -> m WebKitPoint
webkitConvertPointFromPageToNodeUnchecked self node p
  = liftDOM
      ((self ^. jsf "webkitConvertPointFromPageToNode"
          [toJSVal node, toJSVal p])
         >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.webkitConvertPointFromNodeToPage Mozilla Window.webkitConvertPointFromNodeToPage documentation> 
webkitConvertPointFromNodeToPage ::
                                 (MonadDOM m, IsNode node) =>
                                   Window ->
                                     Maybe node -> Maybe WebKitPoint -> m (Maybe WebKitPoint)
webkitConvertPointFromNodeToPage self node p
  = liftDOM
      ((self ^. jsf "webkitConvertPointFromNodeToPage"
          [toJSVal node, toJSVal p])
         >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.webkitConvertPointFromNodeToPage Mozilla Window.webkitConvertPointFromNodeToPage documentation> 
webkitConvertPointFromNodeToPage_ ::
                                  (MonadDOM m, IsNode node) =>
                                    Window -> Maybe node -> Maybe WebKitPoint -> m ()
webkitConvertPointFromNodeToPage_ self node p
  = liftDOM
      (void
         (self ^. jsf "webkitConvertPointFromNodeToPage"
            [toJSVal node, toJSVal p]))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.webkitConvertPointFromNodeToPage Mozilla Window.webkitConvertPointFromNodeToPage documentation> 
webkitConvertPointFromNodeToPageUnsafe ::
                                       (MonadDOM m, IsNode node, HasCallStack) =>
                                         Window -> Maybe node -> Maybe WebKitPoint -> m WebKitPoint
webkitConvertPointFromNodeToPageUnsafe self node p
  = liftDOM
      (((self ^. jsf "webkitConvertPointFromNodeToPage"
           [toJSVal node, toJSVal p])
          >>= fromJSVal)
         >>= maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.webkitConvertPointFromNodeToPage Mozilla Window.webkitConvertPointFromNodeToPage documentation> 
webkitConvertPointFromNodeToPageUnchecked ::
                                          (MonadDOM m, IsNode node) =>
                                            Window ->
                                              Maybe node -> Maybe WebKitPoint -> m WebKitPoint
webkitConvertPointFromNodeToPageUnchecked self node p
  = liftDOM
      ((self ^. jsf "webkitConvertPointFromNodeToPage"
          [toJSVal node, toJSVal p])
         >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.postMessage Mozilla Window.postMessage documentation> 
postMessage ::
            (MonadDOM m, IsSerializedScriptValue message,
             ToJSString targetOrigin, IsArray messagePorts) =>
              Window ->
                Maybe message -> targetOrigin -> Maybe messagePorts -> m ()
postMessage self message targetOrigin messagePorts
  = liftDOM
      (void
         (self ^. jsf "postMessage"
            [toJSVal message, toJSVal targetOrigin, toJSVal messagePorts]))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.requestAnimationFrame Mozilla Window.requestAnimationFrame documentation> 
requestAnimationFrame ::
                      (MonadDOM m) =>
                        Window -> Maybe RequestAnimationFrameCallback -> m Int
requestAnimationFrame self callback
  = liftDOM
      (round <$>
         ((self ^. jsf "requestAnimationFrame" [toJSVal callback]) >>=
            valToNumber))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.requestAnimationFrame Mozilla Window.requestAnimationFrame documentation> 
requestAnimationFrame_ ::
                       (MonadDOM m) =>
                         Window -> Maybe RequestAnimationFrameCallback -> m ()
requestAnimationFrame_ self callback
  = liftDOM
      (void (self ^. jsf "requestAnimationFrame" [toJSVal callback]))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.cancelAnimationFrame Mozilla Window.cancelAnimationFrame documentation> 
cancelAnimationFrame :: (MonadDOM m) => Window -> Int -> m ()
cancelAnimationFrame self id
  = liftDOM (void (self ^. jsf "cancelAnimationFrame" [toJSVal id]))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.webkitRequestAnimationFrame Mozilla Window.webkitRequestAnimationFrame documentation> 
webkitRequestAnimationFrame ::
                            (MonadDOM m) =>
                              Window -> Maybe RequestAnimationFrameCallback -> m Int
webkitRequestAnimationFrame self callback
  = liftDOM
      (round <$>
         ((self ^. jsf "webkitRequestAnimationFrame" [toJSVal callback]) >>=
            valToNumber))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.webkitRequestAnimationFrame Mozilla Window.webkitRequestAnimationFrame documentation> 
webkitRequestAnimationFrame_ ::
                             (MonadDOM m) =>
                               Window -> Maybe RequestAnimationFrameCallback -> m ()
webkitRequestAnimationFrame_ self callback
  = liftDOM
      (void
         (self ^. jsf "webkitRequestAnimationFrame" [toJSVal callback]))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.webkitCancelAnimationFrame Mozilla Window.webkitCancelAnimationFrame documentation> 
webkitCancelAnimationFrame :: (MonadDOM m) => Window -> Int -> m ()
webkitCancelAnimationFrame self id
  = liftDOM
      (void (self ^. jsf "webkitCancelAnimationFrame" [toJSVal id]))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.webkitCancelRequestAnimationFrame Mozilla Window.webkitCancelRequestAnimationFrame documentation> 
webkitCancelRequestAnimationFrame ::
                                  (MonadDOM m) => Window -> Int -> m ()
webkitCancelRequestAnimationFrame self id
  = liftDOM
      (void
         (self ^. jsf "webkitCancelRequestAnimationFrame" [toJSVal id]))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.captureEvents Mozilla Window.captureEvents documentation> 
captureEvents :: (MonadDOM m) => Window -> m ()
captureEvents self
  = liftDOM (void (self ^. jsf "captureEvents" ()))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.releaseEvents Mozilla Window.releaseEvents documentation> 
releaseEvents :: (MonadDOM m) => Window -> m ()
releaseEvents self
  = liftDOM (void (self ^. jsf "releaseEvents" ()))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.webkitIndexedDB Mozilla Window.webkitIndexedDB documentation> 
getWebkitIndexedDB ::
                   (MonadDOM m) => Window -> m (Maybe IDBFactory)
getWebkitIndexedDB self
  = liftDOM ((self ^. js "webkitIndexedDB") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.webkitIndexedDB Mozilla Window.webkitIndexedDB documentation> 
getWebkitIndexedDBUnsafe ::
                         (MonadDOM m, HasCallStack) => Window -> m IDBFactory
getWebkitIndexedDBUnsafe self
  = liftDOM
      (((self ^. js "webkitIndexedDB") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.webkitIndexedDB Mozilla Window.webkitIndexedDB documentation> 
getWebkitIndexedDBUnchecked ::
                            (MonadDOM m) => Window -> m IDBFactory
getWebkitIndexedDBUnchecked self
  = liftDOM ((self ^. js "webkitIndexedDB") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.indexedDB Mozilla Window.indexedDB documentation> 
getIndexedDB :: (MonadDOM m) => Window -> m (Maybe IDBFactory)
getIndexedDB self
  = liftDOM ((self ^. js "indexedDB") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.indexedDB Mozilla Window.indexedDB documentation> 
getIndexedDBUnsafe ::
                   (MonadDOM m, HasCallStack) => Window -> m IDBFactory
getIndexedDBUnsafe self
  = liftDOM
      (((self ^. js "indexedDB") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.indexedDB Mozilla Window.indexedDB documentation> 
getIndexedDBUnchecked :: (MonadDOM m) => Window -> m IDBFactory
getIndexedDBUnchecked self
  = liftDOM ((self ^. js "indexedDB") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.webkitStorageInfo Mozilla Window.webkitStorageInfo documentation> 
getWebkitStorageInfo ::
                     (MonadDOM m) => Window -> m (Maybe StorageInfo)
getWebkitStorageInfo self
  = liftDOM ((self ^. js "webkitStorageInfo") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.webkitStorageInfo Mozilla Window.webkitStorageInfo documentation> 
getWebkitStorageInfoUnsafe ::
                           (MonadDOM m, HasCallStack) => Window -> m StorageInfo
getWebkitStorageInfoUnsafe self
  = liftDOM
      (((self ^. js "webkitStorageInfo") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.webkitStorageInfo Mozilla Window.webkitStorageInfo documentation> 
getWebkitStorageInfoUnchecked ::
                              (MonadDOM m) => Window -> m StorageInfo
getWebkitStorageInfoUnchecked self
  = liftDOM ((self ^. js "webkitStorageInfo") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.speechSynthesis Mozilla Window.speechSynthesis documentation> 
getSpeechSynthesis ::
                   (MonadDOM m) => Window -> m (Maybe SpeechSynthesis)
getSpeechSynthesis self
  = liftDOM ((self ^. js "speechSynthesis") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.speechSynthesis Mozilla Window.speechSynthesis documentation> 
getSpeechSynthesisUnsafe ::
                         (MonadDOM m, HasCallStack) => Window -> m SpeechSynthesis
getSpeechSynthesisUnsafe self
  = liftDOM
      (((self ^. js "speechSynthesis") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.speechSynthesis Mozilla Window.speechSynthesis documentation> 
getSpeechSynthesisUnchecked ::
                            (MonadDOM m) => Window -> m SpeechSynthesis
getSpeechSynthesisUnchecked self
  = liftDOM ((self ^. js "speechSynthesis") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.screen Mozilla Window.screen documentation> 
getScreen :: (MonadDOM m) => Window -> m (Maybe Screen)
getScreen self = liftDOM ((self ^. js "screen") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.screen Mozilla Window.screen documentation> 
getScreenUnsafe :: (MonadDOM m, HasCallStack) => Window -> m Screen
getScreenUnsafe self
  = liftDOM
      (((self ^. js "screen") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.screen Mozilla Window.screen documentation> 
getScreenUnchecked :: (MonadDOM m) => Window -> m Screen
getScreenUnchecked self
  = liftDOM ((self ^. js "screen") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.history Mozilla Window.history documentation> 
getHistory :: (MonadDOM m) => Window -> m (Maybe History)
getHistory self = liftDOM ((self ^. js "history") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.history Mozilla Window.history documentation> 
getHistoryUnsafe ::
                 (MonadDOM m, HasCallStack) => Window -> m History
getHistoryUnsafe self
  = liftDOM
      (((self ^. js "history") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.history Mozilla Window.history documentation> 
getHistoryUnchecked :: (MonadDOM m) => Window -> m History
getHistoryUnchecked self
  = liftDOM ((self ^. js "history") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.locationbar Mozilla Window.locationbar documentation> 
getLocationbar :: (MonadDOM m) => Window -> m (Maybe BarProp)
getLocationbar self
  = liftDOM ((self ^. js "locationbar") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.locationbar Mozilla Window.locationbar documentation> 
getLocationbarUnsafe ::
                     (MonadDOM m, HasCallStack) => Window -> m BarProp
getLocationbarUnsafe self
  = liftDOM
      (((self ^. js "locationbar") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.locationbar Mozilla Window.locationbar documentation> 
getLocationbarUnchecked :: (MonadDOM m) => Window -> m BarProp
getLocationbarUnchecked self
  = liftDOM ((self ^. js "locationbar") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.menubar Mozilla Window.menubar documentation> 
getMenubar :: (MonadDOM m) => Window -> m (Maybe BarProp)
getMenubar self = liftDOM ((self ^. js "menubar") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.menubar Mozilla Window.menubar documentation> 
getMenubarUnsafe ::
                 (MonadDOM m, HasCallStack) => Window -> m BarProp
getMenubarUnsafe self
  = liftDOM
      (((self ^. js "menubar") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.menubar Mozilla Window.menubar documentation> 
getMenubarUnchecked :: (MonadDOM m) => Window -> m BarProp
getMenubarUnchecked self
  = liftDOM ((self ^. js "menubar") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.personalbar Mozilla Window.personalbar documentation> 
getPersonalbar :: (MonadDOM m) => Window -> m (Maybe BarProp)
getPersonalbar self
  = liftDOM ((self ^. js "personalbar") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.personalbar Mozilla Window.personalbar documentation> 
getPersonalbarUnsafe ::
                     (MonadDOM m, HasCallStack) => Window -> m BarProp
getPersonalbarUnsafe self
  = liftDOM
      (((self ^. js "personalbar") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.personalbar Mozilla Window.personalbar documentation> 
getPersonalbarUnchecked :: (MonadDOM m) => Window -> m BarProp
getPersonalbarUnchecked self
  = liftDOM ((self ^. js "personalbar") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.scrollbars Mozilla Window.scrollbars documentation> 
getScrollbars :: (MonadDOM m) => Window -> m (Maybe BarProp)
getScrollbars self
  = liftDOM ((self ^. js "scrollbars") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.scrollbars Mozilla Window.scrollbars documentation> 
getScrollbarsUnsafe ::
                    (MonadDOM m, HasCallStack) => Window -> m BarProp
getScrollbarsUnsafe self
  = liftDOM
      (((self ^. js "scrollbars") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.scrollbars Mozilla Window.scrollbars documentation> 
getScrollbarsUnchecked :: (MonadDOM m) => Window -> m BarProp
getScrollbarsUnchecked self
  = liftDOM ((self ^. js "scrollbars") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.statusbar Mozilla Window.statusbar documentation> 
getStatusbar :: (MonadDOM m) => Window -> m (Maybe BarProp)
getStatusbar self
  = liftDOM ((self ^. js "statusbar") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.statusbar Mozilla Window.statusbar documentation> 
getStatusbarUnsafe ::
                   (MonadDOM m, HasCallStack) => Window -> m BarProp
getStatusbarUnsafe self
  = liftDOM
      (((self ^. js "statusbar") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.statusbar Mozilla Window.statusbar documentation> 
getStatusbarUnchecked :: (MonadDOM m) => Window -> m BarProp
getStatusbarUnchecked self
  = liftDOM ((self ^. js "statusbar") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.toolbar Mozilla Window.toolbar documentation> 
getToolbar :: (MonadDOM m) => Window -> m (Maybe BarProp)
getToolbar self = liftDOM ((self ^. js "toolbar") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.toolbar Mozilla Window.toolbar documentation> 
getToolbarUnsafe ::
                 (MonadDOM m, HasCallStack) => Window -> m BarProp
getToolbarUnsafe self
  = liftDOM
      (((self ^. js "toolbar") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.toolbar Mozilla Window.toolbar documentation> 
getToolbarUnchecked :: (MonadDOM m) => Window -> m BarProp
getToolbarUnchecked self
  = liftDOM ((self ^. js "toolbar") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.navigator Mozilla Window.navigator documentation> 
getNavigator :: (MonadDOM m) => Window -> m (Maybe Navigator)
getNavigator self
  = liftDOM ((self ^. js "navigator") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.navigator Mozilla Window.navigator documentation> 
getNavigatorUnsafe ::
                   (MonadDOM m, HasCallStack) => Window -> m Navigator
getNavigatorUnsafe self
  = liftDOM
      (((self ^. js "navigator") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.navigator Mozilla Window.navigator documentation> 
getNavigatorUnchecked :: (MonadDOM m) => Window -> m Navigator
getNavigatorUnchecked self
  = liftDOM ((self ^. js "navigator") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.clientInformation Mozilla Window.clientInformation documentation> 
getClientInformation ::
                     (MonadDOM m) => Window -> m (Maybe Navigator)
getClientInformation self
  = liftDOM ((self ^. js "clientInformation") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.clientInformation Mozilla Window.clientInformation documentation> 
getClientInformationUnsafe ::
                           (MonadDOM m, HasCallStack) => Window -> m Navigator
getClientInformationUnsafe self
  = liftDOM
      (((self ^. js "clientInformation") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.clientInformation Mozilla Window.clientInformation documentation> 
getClientInformationUnchecked ::
                              (MonadDOM m) => Window -> m Navigator
getClientInformationUnchecked self
  = liftDOM ((self ^. js "clientInformation") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.crypto Mozilla Window.crypto documentation> 
getCrypto :: (MonadDOM m) => Window -> m (Maybe Crypto)
getCrypto self = liftDOM ((self ^. js "crypto") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.crypto Mozilla Window.crypto documentation> 
getCryptoUnsafe :: (MonadDOM m, HasCallStack) => Window -> m Crypto
getCryptoUnsafe self
  = liftDOM
      (((self ^. js "crypto") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.crypto Mozilla Window.crypto documentation> 
getCryptoUnchecked :: (MonadDOM m) => Window -> m Crypto
getCryptoUnchecked self
  = liftDOM ((self ^. js "crypto") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.location Mozilla Window.location documentation> 
setLocation :: (MonadDOM m) => Window -> Maybe Location -> m ()
setLocation self val
  = liftDOM (self ^. jss "location" (toJSVal val))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.location Mozilla Window.location documentation> 
getLocation :: (MonadDOM m) => Window -> m (Maybe Location)
getLocation self = liftDOM ((self ^. js "location") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.location Mozilla Window.location documentation> 
getLocationUnsafe ::
                  (MonadDOM m, HasCallStack) => Window -> m Location
getLocationUnsafe self
  = liftDOM
      (((self ^. js "location") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.location Mozilla Window.location documentation> 
getLocationUnchecked :: (MonadDOM m) => Window -> m Location
getLocationUnchecked self
  = liftDOM ((self ^. js "location") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.event Mozilla Window.event documentation> 
getEvent :: (MonadDOM m) => Window -> m (Maybe Event)
getEvent self = liftDOM ((self ^. js "event") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.event Mozilla Window.event documentation> 
getEventUnsafe :: (MonadDOM m, HasCallStack) => Window -> m Event
getEventUnsafe self
  = liftDOM
      (((self ^. js "event") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.event Mozilla Window.event documentation> 
getEventUnchecked :: (MonadDOM m) => Window -> m Event
getEventUnchecked self
  = liftDOM ((self ^. js "event") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.frameElement Mozilla Window.frameElement documentation> 
getFrameElement :: (MonadDOM m) => Window -> m (Maybe Element)
getFrameElement self
  = liftDOM ((self ^. js "frameElement") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.frameElement Mozilla Window.frameElement documentation> 
getFrameElementUnsafe ::
                      (MonadDOM m, HasCallStack) => Window -> m Element
getFrameElementUnsafe self
  = liftDOM
      (((self ^. js "frameElement") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.frameElement Mozilla Window.frameElement documentation> 
getFrameElementUnchecked :: (MonadDOM m) => Window -> m Element
getFrameElementUnchecked self
  = liftDOM ((self ^. js "frameElement") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.offscreenBuffering Mozilla Window.offscreenBuffering documentation> 
getOffscreenBuffering :: (MonadDOM m) => Window -> m Bool
getOffscreenBuffering self
  = liftDOM ((self ^. js "offscreenBuffering") >>= valToBool)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.outerHeight Mozilla Window.outerHeight documentation> 
getOuterHeight :: (MonadDOM m) => Window -> m Int
getOuterHeight self
  = liftDOM (round <$> ((self ^. js "outerHeight") >>= valToNumber))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.outerWidth Mozilla Window.outerWidth documentation> 
getOuterWidth :: (MonadDOM m) => Window -> m Int
getOuterWidth self
  = liftDOM (round <$> ((self ^. js "outerWidth") >>= valToNumber))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.innerHeight Mozilla Window.innerHeight documentation> 
getInnerHeight :: (MonadDOM m) => Window -> m Int
getInnerHeight self
  = liftDOM (round <$> ((self ^. js "innerHeight") >>= valToNumber))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.innerWidth Mozilla Window.innerWidth documentation> 
getInnerWidth :: (MonadDOM m) => Window -> m Int
getInnerWidth self
  = liftDOM (round <$> ((self ^. js "innerWidth") >>= valToNumber))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.screenX Mozilla Window.screenX documentation> 
getScreenX :: (MonadDOM m) => Window -> m Int
getScreenX self
  = liftDOM (round <$> ((self ^. js "screenX") >>= valToNumber))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.screenY Mozilla Window.screenY documentation> 
getScreenY :: (MonadDOM m) => Window -> m Int
getScreenY self
  = liftDOM (round <$> ((self ^. js "screenY") >>= valToNumber))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.screenLeft Mozilla Window.screenLeft documentation> 
getScreenLeft :: (MonadDOM m) => Window -> m Int
getScreenLeft self
  = liftDOM (round <$> ((self ^. js "screenLeft") >>= valToNumber))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.screenTop Mozilla Window.screenTop documentation> 
getScreenTop :: (MonadDOM m) => Window -> m Int
getScreenTop self
  = liftDOM (round <$> ((self ^. js "screenTop") >>= valToNumber))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.scrollX Mozilla Window.scrollX documentation> 
getScrollX :: (MonadDOM m) => Window -> m Int
getScrollX self
  = liftDOM (round <$> ((self ^. js "scrollX") >>= valToNumber))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.scrollY Mozilla Window.scrollY documentation> 
getScrollY :: (MonadDOM m) => Window -> m Int
getScrollY self
  = liftDOM (round <$> ((self ^. js "scrollY") >>= valToNumber))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.pageXOffset Mozilla Window.pageXOffset documentation> 
getPageXOffset :: (MonadDOM m) => Window -> m Int
getPageXOffset self
  = liftDOM (round <$> ((self ^. js "pageXOffset") >>= valToNumber))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.pageYOffset Mozilla Window.pageYOffset documentation> 
getPageYOffset :: (MonadDOM m) => Window -> m Int
getPageYOffset self
  = liftDOM (round <$> ((self ^. js "pageYOffset") >>= valToNumber))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.closed Mozilla Window.closed documentation> 
getClosed :: (MonadDOM m) => Window -> m Bool
getClosed self = liftDOM ((self ^. js "closed") >>= valToBool)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.length Mozilla Window.length documentation> 
getLength :: (MonadDOM m) => Window -> m Word
getLength self
  = liftDOM (round <$> ((self ^. js "length") >>= valToNumber))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.name Mozilla Window.name documentation> 
setName :: (MonadDOM m, ToJSString val) => Window -> val -> m ()
setName self val = liftDOM (self ^. jss "name" (toJSVal val))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.name Mozilla Window.name documentation> 
getName :: (MonadDOM m, FromJSString result) => Window -> m result
getName self = liftDOM ((self ^. js "name") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.status Mozilla Window.status documentation> 
setStatus :: (MonadDOM m, ToJSString val) => Window -> val -> m ()
setStatus self val = liftDOM (self ^. jss "status" (toJSVal val))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.status Mozilla Window.status documentation> 
getStatus ::
          (MonadDOM m, FromJSString result) => Window -> m result
getStatus self
  = liftDOM ((self ^. js "status") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.defaultStatus Mozilla Window.defaultStatus documentation> 
setDefaultStatus ::
                 (MonadDOM m, ToJSString val) => Window -> val -> m ()
setDefaultStatus self val
  = liftDOM (self ^. jss "defaultStatus" (toJSVal val))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.defaultStatus Mozilla Window.defaultStatus documentation> 
getDefaultStatus ::
                 (MonadDOM m, FromJSString result) => Window -> m result
getDefaultStatus self
  = liftDOM ((self ^. js "defaultStatus") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.defaultstatus Mozilla Window.defaultstatus documentation> 
setDefaultstatus ::
                 (MonadDOM m, ToJSString val) => Window -> val -> m ()
setDefaultstatus self val
  = liftDOM (self ^. jss "defaultstatus" (toJSVal val))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.defaultstatus Mozilla Window.defaultstatus documentation> 
getDefaultstatus ::
                 (MonadDOM m, FromJSString result) => Window -> m result
getDefaultstatus self
  = liftDOM ((self ^. js "defaultstatus") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.self Mozilla Window.self documentation> 
getSelf :: (MonadDOM m) => Window -> m (Maybe Window)
getSelf self = liftDOM ((self ^. js "self") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.self Mozilla Window.self documentation> 
getSelfUnsafe :: (MonadDOM m, HasCallStack) => Window -> m Window
getSelfUnsafe self
  = liftDOM
      (((self ^. js "self") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.self Mozilla Window.self documentation> 
getSelfUnchecked :: (MonadDOM m) => Window -> m Window
getSelfUnchecked self
  = liftDOM ((self ^. js "self") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.window Mozilla Window.window documentation> 
getWindow :: (MonadDOM m) => Window -> m (Maybe Window)
getWindow self = liftDOM ((self ^. js "window") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.window Mozilla Window.window documentation> 
getWindowUnsafe :: (MonadDOM m, HasCallStack) => Window -> m Window
getWindowUnsafe self
  = liftDOM
      (((self ^. js "window") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.window Mozilla Window.window documentation> 
getWindowUnchecked :: (MonadDOM m) => Window -> m Window
getWindowUnchecked self
  = liftDOM ((self ^. js "window") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.frames Mozilla Window.frames documentation> 
getFrames :: (MonadDOM m) => Window -> m (Maybe Window)
getFrames self = liftDOM ((self ^. js "frames") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.frames Mozilla Window.frames documentation> 
getFramesUnsafe :: (MonadDOM m, HasCallStack) => Window -> m Window
getFramesUnsafe self
  = liftDOM
      (((self ^. js "frames") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.frames Mozilla Window.frames documentation> 
getFramesUnchecked :: (MonadDOM m) => Window -> m Window
getFramesUnchecked self
  = liftDOM ((self ^. js "frames") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.opener Mozilla Window.opener documentation> 
getOpener :: (MonadDOM m) => Window -> m (Maybe Window)
getOpener self = liftDOM ((self ^. js "opener") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.opener Mozilla Window.opener documentation> 
getOpenerUnsafe :: (MonadDOM m, HasCallStack) => Window -> m Window
getOpenerUnsafe self
  = liftDOM
      (((self ^. js "opener") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.opener Mozilla Window.opener documentation> 
getOpenerUnchecked :: (MonadDOM m) => Window -> m Window
getOpenerUnchecked self
  = liftDOM ((self ^. js "opener") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.parent Mozilla Window.parent documentation> 
getParent :: (MonadDOM m) => Window -> m (Maybe Window)
getParent self = liftDOM ((self ^. js "parent") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.parent Mozilla Window.parent documentation> 
getParentUnsafe :: (MonadDOM m, HasCallStack) => Window -> m Window
getParentUnsafe self
  = liftDOM
      (((self ^. js "parent") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.parent Mozilla Window.parent documentation> 
getParentUnchecked :: (MonadDOM m) => Window -> m Window
getParentUnchecked self
  = liftDOM ((self ^. js "parent") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.top Mozilla Window.top documentation> 
getTop :: (MonadDOM m) => Window -> m (Maybe Window)
getTop self = liftDOM ((self ^. js "top") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.top Mozilla Window.top documentation> 
getTopUnsafe :: (MonadDOM m, HasCallStack) => Window -> m Window
getTopUnsafe self
  = liftDOM
      (((self ^. js "top") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.top Mozilla Window.top documentation> 
getTopUnchecked :: (MonadDOM m) => Window -> m Window
getTopUnchecked self
  = liftDOM ((self ^. js "top") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.document Mozilla Window.document documentation> 
getDocument :: (MonadDOM m) => Window -> m (Maybe Document)
getDocument self = liftDOM ((self ^. js "document") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.document Mozilla Window.document documentation> 
getDocumentUnsafe ::
                  (MonadDOM m, HasCallStack) => Window -> m Document
getDocumentUnsafe self
  = liftDOM
      (((self ^. js "document") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.document Mozilla Window.document documentation> 
getDocumentUnchecked :: (MonadDOM m) => Window -> m Document
getDocumentUnchecked self
  = liftDOM ((self ^. js "document") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.styleMedia Mozilla Window.styleMedia documentation> 
getStyleMedia :: (MonadDOM m) => Window -> m (Maybe StyleMedia)
getStyleMedia self
  = liftDOM ((self ^. js "styleMedia") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.styleMedia Mozilla Window.styleMedia documentation> 
getStyleMediaUnsafe ::
                    (MonadDOM m, HasCallStack) => Window -> m StyleMedia
getStyleMediaUnsafe self
  = liftDOM
      (((self ^. js "styleMedia") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.styleMedia Mozilla Window.styleMedia documentation> 
getStyleMediaUnchecked :: (MonadDOM m) => Window -> m StyleMedia
getStyleMediaUnchecked self
  = liftDOM ((self ^. js "styleMedia") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.devicePixelRatio Mozilla Window.devicePixelRatio documentation> 
getDevicePixelRatio :: (MonadDOM m) => Window -> m Double
getDevicePixelRatio self
  = liftDOM ((self ^. js "devicePixelRatio") >>= valToNumber)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.applicationCache Mozilla Window.applicationCache documentation> 
getApplicationCache ::
                    (MonadDOM m) => Window -> m (Maybe ApplicationCache)
getApplicationCache self
  = liftDOM ((self ^. js "applicationCache") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.applicationCache Mozilla Window.applicationCache documentation> 
getApplicationCacheUnsafe ::
                          (MonadDOM m, HasCallStack) => Window -> m ApplicationCache
getApplicationCacheUnsafe self
  = liftDOM
      (((self ^. js "applicationCache") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.applicationCache Mozilla Window.applicationCache documentation> 
getApplicationCacheUnchecked ::
                             (MonadDOM m) => Window -> m ApplicationCache
getApplicationCacheUnchecked self
  = liftDOM ((self ^. js "applicationCache") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.sessionStorage Mozilla Window.sessionStorage documentation> 
getSessionStorage :: (MonadDOM m) => Window -> m (Maybe Storage)
getSessionStorage self
  = liftDOM ((self ^. js "sessionStorage") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.sessionStorage Mozilla Window.sessionStorage documentation> 
getSessionStorageUnsafe ::
                        (MonadDOM m, HasCallStack) => Window -> m Storage
getSessionStorageUnsafe self
  = liftDOM
      (((self ^. js "sessionStorage") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.sessionStorage Mozilla Window.sessionStorage documentation> 
getSessionStorageUnchecked :: (MonadDOM m) => Window -> m Storage
getSessionStorageUnchecked self
  = liftDOM ((self ^. js "sessionStorage") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.localStorage Mozilla Window.localStorage documentation> 
getLocalStorage :: (MonadDOM m) => Window -> m (Maybe Storage)
getLocalStorage self
  = liftDOM ((self ^. js "localStorage") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.localStorage Mozilla Window.localStorage documentation> 
getLocalStorageUnsafe ::
                      (MonadDOM m, HasCallStack) => Window -> m Storage
getLocalStorageUnsafe self
  = liftDOM
      (((self ^. js "localStorage") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.localStorage Mozilla Window.localStorage documentation> 
getLocalStorageUnchecked :: (MonadDOM m) => Window -> m Storage
getLocalStorageUnchecked self
  = liftDOM ((self ^. js "localStorage") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.orientation Mozilla Window.orientation documentation> 
getOrientation :: (MonadDOM m) => Window -> m Int
getOrientation self
  = liftDOM (round <$> ((self ^. js "orientation") >>= valToNumber))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.performance Mozilla Window.performance documentation> 
getPerformance :: (MonadDOM m) => Window -> m (Maybe Performance)
getPerformance self
  = liftDOM ((self ^. js "performance") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.performance Mozilla Window.performance documentation> 
getPerformanceUnsafe ::
                     (MonadDOM m, HasCallStack) => Window -> m Performance
getPerformanceUnsafe self
  = liftDOM
      (((self ^. js "performance") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.performance Mozilla Window.performance documentation> 
getPerformanceUnchecked :: (MonadDOM m) => Window -> m Performance
getPerformanceUnchecked self
  = liftDOM ((self ^. js "performance") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.CSS Mozilla Window.CSS documentation> 
getCSS :: (MonadDOM m) => Window -> m (Maybe CSS)
getCSS self = liftDOM ((self ^. js "CSS") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.CSS Mozilla Window.CSS documentation> 
getCSSUnsafe :: (MonadDOM m, HasCallStack) => Window -> m CSS
getCSSUnsafe self
  = liftDOM
      (((self ^. js "CSS") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.CSS Mozilla Window.CSS documentation> 
getCSSUnchecked :: (MonadDOM m) => Window -> m CSS
getCSSUnchecked self
  = liftDOM ((self ^. js "CSS") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onabort Mozilla Window.onabort documentation> 
abort :: EventName Window UIEvent
abort = unsafeEventName (toJSString "abort")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onbeforeunload Mozilla Window.onbeforeunload documentation> 
beforeUnload :: EventName Window BeforeUnloadEvent
beforeUnload = unsafeEventName (toJSString "beforeunload")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onblur Mozilla Window.onblur documentation> 
blurEvent :: EventName Window FocusEvent
blurEvent = unsafeEventName (toJSString "blur")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.oncanplay Mozilla Window.oncanplay documentation> 
canPlay :: EventName Window Event
canPlay = unsafeEventName (toJSString "canplay")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.oncanplaythrough Mozilla Window.oncanplaythrough documentation> 
canPlayThrough :: EventName Window Event
canPlayThrough = unsafeEventName (toJSString "canplaythrough")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onchange Mozilla Window.onchange documentation> 
change :: EventName Window Event
change = unsafeEventName (toJSString "change")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onclick Mozilla Window.onclick documentation> 
click :: EventName Window MouseEvent
click = unsafeEventName (toJSString "click")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.oncontextmenu Mozilla Window.oncontextmenu documentation> 
contextMenu :: EventName Window MouseEvent
contextMenu = unsafeEventName (toJSString "contextmenu")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.ondblclick Mozilla Window.ondblclick documentation> 
dblClick :: EventName Window MouseEvent
dblClick = unsafeEventName (toJSString "dblclick")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.ondrag Mozilla Window.ondrag documentation> 
drag :: EventName Window MouseEvent
drag = unsafeEventName (toJSString "drag")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.ondragend Mozilla Window.ondragend documentation> 
dragEnd :: EventName Window MouseEvent
dragEnd = unsafeEventName (toJSString "dragend")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.ondragenter Mozilla Window.ondragenter documentation> 
dragEnter :: EventName Window MouseEvent
dragEnter = unsafeEventName (toJSString "dragenter")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.ondragleave Mozilla Window.ondragleave documentation> 
dragLeave :: EventName Window MouseEvent
dragLeave = unsafeEventName (toJSString "dragleave")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.ondragover Mozilla Window.ondragover documentation> 
dragOver :: EventName Window MouseEvent
dragOver = unsafeEventName (toJSString "dragover")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.ondragstart Mozilla Window.ondragstart documentation> 
dragStart :: EventName Window MouseEvent
dragStart = unsafeEventName (toJSString "dragstart")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.ondrop Mozilla Window.ondrop documentation> 
drop :: EventName Window MouseEvent
drop = unsafeEventName (toJSString "drop")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.ondurationchange Mozilla Window.ondurationchange documentation> 
durationChange :: EventName Window Event
durationChange = unsafeEventName (toJSString "durationchange")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onemptied Mozilla Window.onemptied documentation> 
emptied :: EventName Window Event
emptied = unsafeEventName (toJSString "emptied")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onended Mozilla Window.onended documentation> 
ended :: EventName Window Event
ended = unsafeEventName (toJSString "ended")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onerror Mozilla Window.onerror documentation> 
error :: EventName Window UIEvent
error = unsafeEventName (toJSString "error")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onfocus Mozilla Window.onfocus documentation> 
focusEvent :: EventName Window FocusEvent
focusEvent = unsafeEventName (toJSString "focus")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onhashchange Mozilla Window.onhashchange documentation> 
hashChange :: EventName Window HashChangeEvent
hashChange = unsafeEventName (toJSString "hashchange")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.oninput Mozilla Window.oninput documentation> 
input :: EventName Window Event
input = unsafeEventName (toJSString "input")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.oninvalid Mozilla Window.oninvalid documentation> 
invalid :: EventName Window Event
invalid = unsafeEventName (toJSString "invalid")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onkeydown Mozilla Window.onkeydown documentation> 
keyDown :: EventName Window KeyboardEvent
keyDown = unsafeEventName (toJSString "keydown")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onkeypress Mozilla Window.onkeypress documentation> 
keyPress :: EventName Window KeyboardEvent
keyPress = unsafeEventName (toJSString "keypress")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onkeyup Mozilla Window.onkeyup documentation> 
keyUp :: EventName Window KeyboardEvent
keyUp = unsafeEventName (toJSString "keyup")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onload Mozilla Window.onload documentation> 
load :: EventName Window UIEvent
load = unsafeEventName (toJSString "load")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onloadeddata Mozilla Window.onloadeddata documentation> 
loadedData :: EventName Window Event
loadedData = unsafeEventName (toJSString "loadeddata")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onloadedmetadata Mozilla Window.onloadedmetadata documentation> 
loadedMetadata :: EventName Window Event
loadedMetadata = unsafeEventName (toJSString "loadedmetadata")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onloadstart Mozilla Window.onloadstart documentation> 
loadStart :: EventName Window ProgressEvent
loadStart = unsafeEventName (toJSString "loadstart")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onmessage Mozilla Window.onmessage documentation> 
message :: EventName Window MessageEvent
message = unsafeEventName (toJSString "message")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onmousedown Mozilla Window.onmousedown documentation> 
mouseDown :: EventName Window MouseEvent
mouseDown = unsafeEventName (toJSString "mousedown")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onmouseenter Mozilla Window.onmouseenter documentation> 
mouseEnter :: EventName Window MouseEvent
mouseEnter = unsafeEventName (toJSString "mouseenter")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onmouseleave Mozilla Window.onmouseleave documentation> 
mouseLeave :: EventName Window MouseEvent
mouseLeave = unsafeEventName (toJSString "mouseleave")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onmousemove Mozilla Window.onmousemove documentation> 
mouseMove :: EventName Window MouseEvent
mouseMove = unsafeEventName (toJSString "mousemove")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onmouseout Mozilla Window.onmouseout documentation> 
mouseOut :: EventName Window MouseEvent
mouseOut = unsafeEventName (toJSString "mouseout")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onmouseover Mozilla Window.onmouseover documentation> 
mouseOver :: EventName Window MouseEvent
mouseOver = unsafeEventName (toJSString "mouseover")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onmouseup Mozilla Window.onmouseup documentation> 
mouseUp :: EventName Window MouseEvent
mouseUp = unsafeEventName (toJSString "mouseup")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onmousewheel Mozilla Window.onmousewheel documentation> 
mouseWheel :: EventName Window MouseEvent
mouseWheel = unsafeEventName (toJSString "mousewheel")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onoffline Mozilla Window.onoffline documentation> 
offline :: EventName Window Event
offline = unsafeEventName (toJSString "offline")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.ononline Mozilla Window.ononline documentation> 
online :: EventName Window Event
online = unsafeEventName (toJSString "online")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onpagehide Mozilla Window.onpagehide documentation> 
pageHide :: EventName Window PageTransitionEvent
pageHide = unsafeEventName (toJSString "pagehide")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onpageshow Mozilla Window.onpageshow documentation> 
pageShow :: EventName Window PageTransitionEvent
pageShow = unsafeEventName (toJSString "pageshow")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onpause Mozilla Window.onpause documentation> 
pause :: EventName Window Event
pause = unsafeEventName (toJSString "pause")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onplay Mozilla Window.onplay documentation> 
play :: EventName Window Event
play = unsafeEventName (toJSString "play")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onplaying Mozilla Window.onplaying documentation> 
playing :: EventName Window Event
playing = unsafeEventName (toJSString "playing")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onpopstate Mozilla Window.onpopstate documentation> 
popState :: EventName Window PopStateEvent
popState = unsafeEventName (toJSString "popstate")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onprogress Mozilla Window.onprogress documentation> 
progress :: EventName Window ProgressEvent
progress = unsafeEventName (toJSString "progress")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onratechange Mozilla Window.onratechange documentation> 
rateChange :: EventName Window Event
rateChange = unsafeEventName (toJSString "ratechange")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onresize Mozilla Window.onresize documentation> 
resize :: EventName Window UIEvent
resize = unsafeEventName (toJSString "resize")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onscroll Mozilla Window.onscroll documentation> 
scrollEvent :: EventName Window UIEvent
scrollEvent = unsafeEventName (toJSString "scroll")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onseeked Mozilla Window.onseeked documentation> 
seeked :: EventName Window Event
seeked = unsafeEventName (toJSString "seeked")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onseeking Mozilla Window.onseeking documentation> 
seeking :: EventName Window Event
seeking = unsafeEventName (toJSString "seeking")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onselect Mozilla Window.onselect documentation> 
select :: EventName Window UIEvent
select = unsafeEventName (toJSString "select")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onstalled Mozilla Window.onstalled documentation> 
stalled :: EventName Window Event
stalled = unsafeEventName (toJSString "stalled")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onstorage Mozilla Window.onstorage documentation> 
storage :: EventName Window StorageEvent
storage = unsafeEventName (toJSString "storage")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onsubmit Mozilla Window.onsubmit documentation> 
submit :: EventName Window Event
submit = unsafeEventName (toJSString "submit")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onsuspend Mozilla Window.onsuspend documentation> 
suspend :: EventName Window Event
suspend = unsafeEventName (toJSString "suspend")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.ontimeupdate Mozilla Window.ontimeupdate documentation> 
timeUpdate :: EventName Window Event
timeUpdate = unsafeEventName (toJSString "timeupdate")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onunload Mozilla Window.onunload documentation> 
unload :: EventName Window UIEvent
unload = unsafeEventName (toJSString "unload")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onvolumechange Mozilla Window.onvolumechange documentation> 
volumeChange :: EventName Window Event
volumeChange = unsafeEventName (toJSString "volumechange")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onwaiting Mozilla Window.onwaiting documentation> 
waiting :: EventName Window Event
waiting = unsafeEventName (toJSString "waiting")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onwheel Mozilla Window.onwheel documentation> 
wheel :: EventName Window WheelEvent
wheel = unsafeEventName (toJSString "wheel")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onreset Mozilla Window.onreset documentation> 
reset :: EventName Window Event
reset = unsafeEventName (toJSString "reset")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onsearch Mozilla Window.onsearch documentation> 
search :: EventName Window Event
search = unsafeEventName (toJSString "search")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onwebkitanimationend Mozilla Window.onwebkitanimationend documentation> 
webKitAnimationEnd :: EventName Window AnimationEvent
webKitAnimationEnd
  = unsafeEventName (toJSString "webkitanimationend")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onwebkitanimationiteration Mozilla Window.onwebkitanimationiteration documentation> 
webKitAnimationIteration :: EventName Window AnimationEvent
webKitAnimationIteration
  = unsafeEventName (toJSString "webkitanimationiteration")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onwebkitanimationstart Mozilla Window.onwebkitanimationstart documentation> 
webKitAnimationStart :: EventName Window AnimationEvent
webKitAnimationStart
  = unsafeEventName (toJSString "webkitanimationstart")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onanimationend Mozilla Window.onanimationend documentation> 
animationEnd :: EventName Window AnimationEvent
animationEnd = unsafeEventName (toJSString "animationend")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onanimationiteration Mozilla Window.onanimationiteration documentation> 
animationIteration :: EventName Window AnimationEvent
animationIteration
  = unsafeEventName (toJSString "animationiteration")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onanimationstart Mozilla Window.onanimationstart documentation> 
animationStart :: EventName Window AnimationEvent
animationStart = unsafeEventName (toJSString "animationstart")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onwebkittransitionend Mozilla Window.onwebkittransitionend documentation> 
webKitTransitionEnd :: EventName Window TransitionEvent
webKitTransitionEnd
  = unsafeEventName (toJSString "webkittransitionend")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.ontransitionend Mozilla Window.ontransitionend documentation> 
transitionEnd :: EventName Window TransitionEvent
transitionEnd = unsafeEventName (toJSString "transitionend")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onorientationchange Mozilla Window.onorientationchange documentation> 
orientationChange :: EventName Window Event
orientationChange
  = unsafeEventName (toJSString "orientationchange")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.ontouchstart Mozilla Window.ontouchstart documentation> 
touchStart :: EventName Window TouchEvent
touchStart = unsafeEventName (toJSString "touchstart")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.ontouchmove Mozilla Window.ontouchmove documentation> 
touchMove :: EventName Window TouchEvent
touchMove = unsafeEventName (toJSString "touchmove")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.ontouchend Mozilla Window.ontouchend documentation> 
touchEnd :: EventName Window TouchEvent
touchEnd = unsafeEventName (toJSString "touchend")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.ontouchcancel Mozilla Window.ontouchcancel documentation> 
touchCancel :: EventName Window TouchEvent
touchCancel = unsafeEventName (toJSString "touchcancel")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.ongesturestart Mozilla Window.ongesturestart documentation> 
gestureStart :: EventName Window UIEvent
gestureStart = unsafeEventName (toJSString "gesturestart")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.ongesturechange Mozilla Window.ongesturechange documentation> 
gestureChange :: EventName Window UIEvent
gestureChange = unsafeEventName (toJSString "gesturechange")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.ongestureend Mozilla Window.ongestureend documentation> 
gestureEnd :: EventName Window UIEvent
gestureEnd = unsafeEventName (toJSString "gestureend")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.ondevicemotion Mozilla Window.ondevicemotion documentation> 
deviceMotion :: EventName Window DeviceMotionEvent
deviceMotion = unsafeEventName (toJSString "devicemotion")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.ondeviceorientation Mozilla Window.ondeviceorientation documentation> 
deviceOrientation :: EventName Window DeviceOrientationEvent
deviceOrientation
  = unsafeEventName (toJSString "deviceorientation")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onwebkitdeviceproximity Mozilla Window.onwebkitdeviceproximity documentation> 
webKitDeviceProximity :: EventName Window DeviceProximityEvent
webKitDeviceProximity
  = unsafeEventName (toJSString "webkitdeviceproximity")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onwebkitwillrevealbottom Mozilla Window.onwebkitwillrevealbottom documentation> 
webKitWillRevealBottom :: EventName Window Event
webKitWillRevealBottom
  = unsafeEventName (toJSString "webkitwillrevealbottom")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onwebkitwillrevealleft Mozilla Window.onwebkitwillrevealleft documentation> 
webKitWillRevealLeft :: EventName Window Event
webKitWillRevealLeft
  = unsafeEventName (toJSString "webkitwillrevealleft")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onwebkitwillrevealright Mozilla Window.onwebkitwillrevealright documentation> 
webKitWillRevealRight :: EventName Window Event
webKitWillRevealRight
  = unsafeEventName (toJSString "webkitwillrevealright")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.onwebkitwillrevealtop Mozilla Window.onwebkitwillrevealtop documentation> 
webKitWillRevealTop :: EventName Window Event
webKitWillRevealTop
  = unsafeEventName (toJSString "webkitwillrevealtop")
