{-# LANGUAGE PatternSynonyms #-}
-- For HasCallStack compatibility
{-# LANGUAGE ImplicitParams, ConstraintKinds, KindSignatures #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module JSDOM.Generated.MediaControlsHost
       (sortedTrackListForMenu, sortedTrackListForMenu_,
        sortedTrackListForMenuAudio, sortedTrackListForMenuAudio_,
        displayNameForTrack, displayNameForTrack_,
        displayNameForTrackAudio, displayNameForTrackAudio_,
        setSelectedTextTrack, updateTextTrackContainer, enteredFullscreen,
        exitedFullscreen, enterFullscreenOptimized, mediaUIImageData,
        mediaUIImageData_, getCaptionMenuOffItem,
        getCaptionMenuOffItemUnsafe, getCaptionMenuOffItemUnchecked,
        getCaptionMenuAutomaticItem, getCaptionMenuAutomaticItemUnsafe,
        getCaptionMenuAutomaticItemUnchecked, getCaptionDisplayMode,
        getTextTrackContainer, getTextTrackContainerUnsafe,
        getTextTrackContainerUnchecked, getMediaPlaybackAllowsInline,
        getSupportsFullscreen, getUserGestureRequired,
        getExternalDeviceDisplayName, getExternalDeviceType,
        setControlsDependOnPageScaleFactor,
        getControlsDependOnPageScaleFactor, MediaControlsHost(..),
        gTypeMediaControlsHost)
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

-- | <https://developer.mozilla.org/en-US/docs/Web/API/MediaControlsHost.sortedTrackListForMenu Mozilla MediaControlsHost.sortedTrackListForMenu documentation> 
sortedTrackListForMenu ::
                       (MonadDOM m) =>
                         MediaControlsHost -> Maybe TextTrackList -> m [Maybe TextTrack]
sortedTrackListForMenu self trackList
  = liftDOM
      ((self ^. jsf "sortedTrackListForMenu" [toJSVal trackList]) >>=
         fromJSArray)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/MediaControlsHost.sortedTrackListForMenu Mozilla MediaControlsHost.sortedTrackListForMenu documentation> 
sortedTrackListForMenu_ ::
                        (MonadDOM m) => MediaControlsHost -> Maybe TextTrackList -> m ()
sortedTrackListForMenu_ self trackList
  = liftDOM
      (void (self ^. jsf "sortedTrackListForMenu" [toJSVal trackList]))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/MediaControlsHost.sortedTrackListForMenu Mozilla MediaControlsHost.sortedTrackListForMenu documentation> 
sortedTrackListForMenuAudio ::
                            (MonadDOM m) =>
                              MediaControlsHost -> Maybe AudioTrackList -> m [Maybe AudioTrack]
sortedTrackListForMenuAudio self trackList
  = liftDOM
      ((self ^. jsf "sortedTrackListForMenu" [toJSVal trackList]) >>=
         fromJSArray)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/MediaControlsHost.sortedTrackListForMenu Mozilla MediaControlsHost.sortedTrackListForMenu documentation> 
sortedTrackListForMenuAudio_ ::
                             (MonadDOM m) => MediaControlsHost -> Maybe AudioTrackList -> m ()
sortedTrackListForMenuAudio_ self trackList
  = liftDOM
      (void (self ^. jsf "sortedTrackListForMenu" [toJSVal trackList]))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/MediaControlsHost.displayNameForTrack Mozilla MediaControlsHost.displayNameForTrack documentation> 
displayNameForTrack ::
                    (MonadDOM m, FromJSString result) =>
                      MediaControlsHost -> Maybe TextTrack -> m result
displayNameForTrack self track
  = liftDOM
      ((self ^. jsf "displayNameForTrack" [toJSVal track]) >>=
         fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/MediaControlsHost.displayNameForTrack Mozilla MediaControlsHost.displayNameForTrack documentation> 
displayNameForTrack_ ::
                     (MonadDOM m) => MediaControlsHost -> Maybe TextTrack -> m ()
displayNameForTrack_ self track
  = liftDOM
      (void (self ^. jsf "displayNameForTrack" [toJSVal track]))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/MediaControlsHost.displayNameForTrack Mozilla MediaControlsHost.displayNameForTrack documentation> 
displayNameForTrackAudio ::
                         (MonadDOM m, FromJSString result) =>
                           MediaControlsHost -> Maybe AudioTrack -> m result
displayNameForTrackAudio self track
  = liftDOM
      ((self ^. jsf "displayNameForTrack" [toJSVal track]) >>=
         fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/MediaControlsHost.displayNameForTrack Mozilla MediaControlsHost.displayNameForTrack documentation> 
displayNameForTrackAudio_ ::
                          (MonadDOM m) => MediaControlsHost -> Maybe AudioTrack -> m ()
displayNameForTrackAudio_ self track
  = liftDOM
      (void (self ^. jsf "displayNameForTrack" [toJSVal track]))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/MediaControlsHost.setSelectedTextTrack Mozilla MediaControlsHost.setSelectedTextTrack documentation> 
setSelectedTextTrack ::
                     (MonadDOM m) => MediaControlsHost -> Maybe TextTrack -> m ()
setSelectedTextTrack self track
  = liftDOM
      (void (self ^. jsf "setSelectedTextTrack" [toJSVal track]))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/MediaControlsHost.updateTextTrackContainer Mozilla MediaControlsHost.updateTextTrackContainer documentation> 
updateTextTrackContainer ::
                         (MonadDOM m) => MediaControlsHost -> m ()
updateTextTrackContainer self
  = liftDOM (void (self ^. jsf "updateTextTrackContainer" ()))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/MediaControlsHost.enteredFullscreen Mozilla MediaControlsHost.enteredFullscreen documentation> 
enteredFullscreen :: (MonadDOM m) => MediaControlsHost -> m ()
enteredFullscreen self
  = liftDOM (void (self ^. jsf "enteredFullscreen" ()))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/MediaControlsHost.exitedFullscreen Mozilla MediaControlsHost.exitedFullscreen documentation> 
exitedFullscreen :: (MonadDOM m) => MediaControlsHost -> m ()
exitedFullscreen self
  = liftDOM (void (self ^. jsf "exitedFullscreen" ()))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/MediaControlsHost.enterFullscreenOptimized Mozilla MediaControlsHost.enterFullscreenOptimized documentation> 
enterFullscreenOptimized ::
                         (MonadDOM m) => MediaControlsHost -> m ()
enterFullscreenOptimized self
  = liftDOM (void (self ^. jsf "enterFullscreenOptimized" ()))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/MediaControlsHost.mediaUIImageData Mozilla MediaControlsHost.mediaUIImageData documentation> 
mediaUIImageData ::
                 (MonadDOM m, FromJSString result) =>
                   MediaControlsHost -> MediaUIPartID -> m result
mediaUIImageData self partID
  = liftDOM
      ((self ^. jsf "mediaUIImageData" [toJSVal partID]) >>=
         fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/MediaControlsHost.mediaUIImageData Mozilla MediaControlsHost.mediaUIImageData documentation> 
mediaUIImageData_ ::
                  (MonadDOM m) => MediaControlsHost -> MediaUIPartID -> m ()
mediaUIImageData_ self partID
  = liftDOM (void (self ^. jsf "mediaUIImageData" [toJSVal partID]))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/MediaControlsHost.captionMenuOffItem Mozilla MediaControlsHost.captionMenuOffItem documentation> 
getCaptionMenuOffItem ::
                      (MonadDOM m) => MediaControlsHost -> m (Maybe TextTrack)
getCaptionMenuOffItem self
  = liftDOM ((self ^. js "captionMenuOffItem") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/MediaControlsHost.captionMenuOffItem Mozilla MediaControlsHost.captionMenuOffItem documentation> 
getCaptionMenuOffItemUnsafe ::
                            (MonadDOM m, HasCallStack) => MediaControlsHost -> m TextTrack
getCaptionMenuOffItemUnsafe self
  = liftDOM
      (((self ^. js "captionMenuOffItem") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/MediaControlsHost.captionMenuOffItem Mozilla MediaControlsHost.captionMenuOffItem documentation> 
getCaptionMenuOffItemUnchecked ::
                               (MonadDOM m) => MediaControlsHost -> m TextTrack
getCaptionMenuOffItemUnchecked self
  = liftDOM
      ((self ^. js "captionMenuOffItem") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/MediaControlsHost.captionMenuAutomaticItem Mozilla MediaControlsHost.captionMenuAutomaticItem documentation> 
getCaptionMenuAutomaticItem ::
                            (MonadDOM m) => MediaControlsHost -> m (Maybe TextTrack)
getCaptionMenuAutomaticItem self
  = liftDOM ((self ^. js "captionMenuAutomaticItem") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/MediaControlsHost.captionMenuAutomaticItem Mozilla MediaControlsHost.captionMenuAutomaticItem documentation> 
getCaptionMenuAutomaticItemUnsafe ::
                                  (MonadDOM m, HasCallStack) => MediaControlsHost -> m TextTrack
getCaptionMenuAutomaticItemUnsafe self
  = liftDOM
      (((self ^. js "captionMenuAutomaticItem") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/MediaControlsHost.captionMenuAutomaticItem Mozilla MediaControlsHost.captionMenuAutomaticItem documentation> 
getCaptionMenuAutomaticItemUnchecked ::
                                     (MonadDOM m) => MediaControlsHost -> m TextTrack
getCaptionMenuAutomaticItemUnchecked self
  = liftDOM
      ((self ^. js "captionMenuAutomaticItem") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/MediaControlsHost.captionDisplayMode Mozilla MediaControlsHost.captionDisplayMode documentation> 
getCaptionDisplayMode ::
                      (MonadDOM m, FromJSString result) => MediaControlsHost -> m result
getCaptionDisplayMode self
  = liftDOM
      ((self ^. js "captionDisplayMode") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/MediaControlsHost.textTrackContainer Mozilla MediaControlsHost.textTrackContainer documentation> 
getTextTrackContainer ::
                      (MonadDOM m) => MediaControlsHost -> m (Maybe HTMLElement)
getTextTrackContainer self
  = liftDOM ((self ^. js "textTrackContainer") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/MediaControlsHost.textTrackContainer Mozilla MediaControlsHost.textTrackContainer documentation> 
getTextTrackContainerUnsafe ::
                            (MonadDOM m, HasCallStack) => MediaControlsHost -> m HTMLElement
getTextTrackContainerUnsafe self
  = liftDOM
      (((self ^. js "textTrackContainer") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/MediaControlsHost.textTrackContainer Mozilla MediaControlsHost.textTrackContainer documentation> 
getTextTrackContainerUnchecked ::
                               (MonadDOM m) => MediaControlsHost -> m HTMLElement
getTextTrackContainerUnchecked self
  = liftDOM
      ((self ^. js "textTrackContainer") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/MediaControlsHost.mediaPlaybackAllowsInline Mozilla MediaControlsHost.mediaPlaybackAllowsInline documentation> 
getMediaPlaybackAllowsInline ::
                             (MonadDOM m) => MediaControlsHost -> m Bool
getMediaPlaybackAllowsInline self
  = liftDOM ((self ^. js "mediaPlaybackAllowsInline") >>= valToBool)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/MediaControlsHost.supportsFullscreen Mozilla MediaControlsHost.supportsFullscreen documentation> 
getSupportsFullscreen ::
                      (MonadDOM m) => MediaControlsHost -> m Bool
getSupportsFullscreen self
  = liftDOM ((self ^. js "supportsFullscreen") >>= valToBool)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/MediaControlsHost.userGestureRequired Mozilla MediaControlsHost.userGestureRequired documentation> 
getUserGestureRequired ::
                       (MonadDOM m) => MediaControlsHost -> m Bool
getUserGestureRequired self
  = liftDOM ((self ^. js "userGestureRequired") >>= valToBool)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/MediaControlsHost.externalDeviceDisplayName Mozilla MediaControlsHost.externalDeviceDisplayName documentation> 
getExternalDeviceDisplayName ::
                             (MonadDOM m, FromJSString result) => MediaControlsHost -> m result
getExternalDeviceDisplayName self
  = liftDOM
      ((self ^. js "externalDeviceDisplayName") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/MediaControlsHost.externalDeviceType Mozilla MediaControlsHost.externalDeviceType documentation> 
getExternalDeviceType ::
                      (MonadDOM m) => MediaControlsHost -> m DeviceType
getExternalDeviceType self
  = liftDOM
      ((self ^. js "externalDeviceType") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/MediaControlsHost.controlsDependOnPageScaleFactor Mozilla MediaControlsHost.controlsDependOnPageScaleFactor documentation> 
setControlsDependOnPageScaleFactor ::
                                   (MonadDOM m) => MediaControlsHost -> Bool -> m ()
setControlsDependOnPageScaleFactor self val
  = liftDOM
      (self ^. jss "controlsDependOnPageScaleFactor" (toJSVal val))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/MediaControlsHost.controlsDependOnPageScaleFactor Mozilla MediaControlsHost.controlsDependOnPageScaleFactor documentation> 
getControlsDependOnPageScaleFactor ::
                                   (MonadDOM m) => MediaControlsHost -> m Bool
getControlsDependOnPageScaleFactor self
  = liftDOM
      ((self ^. js "controlsDependOnPageScaleFactor") >>= valToBool)
