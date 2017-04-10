{-# LANGUAGE PatternSynonyms #-}
-- For HasCallStack compatibility
{-# LANGUAGE ImplicitParams, ConstraintKinds, KindSignatures #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module JSDOM.Generated.HTMLAnchorElement
       (toString, toString_, setCharset, getCharset, setCoords, getCoords,
        setDownload, getDownload, setHref, getHref, setHreflang,
        getHreflang, setName, getName, setPing, getPing, setRel, getRel,
        setRev, getRev, setShape, getShape, setTarget, getTarget, setType,
        getType, setHash, getHash, getHashUnsafe, getHashUnchecked,
        setHost, getHost, getHostUnsafe, getHostUnchecked, setHostname,
        getHostname, getHostnameUnsafe, getHostnameUnchecked, setPathname,
        getPathname, getPathnameUnsafe, getPathnameUnchecked, setPort,
        getPort, getPortUnsafe, getPortUnchecked, setProtocol, getProtocol,
        getProtocolUnsafe, getProtocolUnchecked, setSearch, getSearch,
        getSearchUnsafe, getSearchUnchecked, getOrigin, getOriginUnsafe,
        getOriginUnchecked, setText, getText, getRelList, getRelListUnsafe,
        getRelListUnchecked, HTMLAnchorElement(..), gTypeHTMLAnchorElement)
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

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.toString Mozilla HTMLAnchorElement.toString documentation> 
toString ::
         (MonadDOM m, FromJSString result) => HTMLAnchorElement -> m result
toString self
  = liftDOM ((self ^. jsf "toString" ()) >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.toString Mozilla HTMLAnchorElement.toString documentation> 
toString_ :: (MonadDOM m) => HTMLAnchorElement -> m ()
toString_ self = liftDOM (void (self ^. jsf "toString" ()))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.charset Mozilla HTMLAnchorElement.charset documentation> 
setCharset ::
           (MonadDOM m, ToJSString val) => HTMLAnchorElement -> val -> m ()
setCharset self val = liftDOM (self ^. jss "charset" (toJSVal val))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.charset Mozilla HTMLAnchorElement.charset documentation> 
getCharset ::
           (MonadDOM m, FromJSString result) => HTMLAnchorElement -> m result
getCharset self
  = liftDOM ((self ^. js "charset") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.coords Mozilla HTMLAnchorElement.coords documentation> 
setCoords ::
          (MonadDOM m, ToJSString val) => HTMLAnchorElement -> val -> m ()
setCoords self val = liftDOM (self ^. jss "coords" (toJSVal val))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.coords Mozilla HTMLAnchorElement.coords documentation> 
getCoords ::
          (MonadDOM m, FromJSString result) => HTMLAnchorElement -> m result
getCoords self
  = liftDOM ((self ^. js "coords") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.download Mozilla HTMLAnchorElement.download documentation> 
setDownload ::
            (MonadDOM m, ToJSString val) => HTMLAnchorElement -> val -> m ()
setDownload self val
  = liftDOM (self ^. jss "download" (toJSVal val))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.download Mozilla HTMLAnchorElement.download documentation> 
getDownload ::
            (MonadDOM m, FromJSString result) => HTMLAnchorElement -> m result
getDownload self
  = liftDOM ((self ^. js "download") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.href Mozilla HTMLAnchorElement.href documentation> 
setHref ::
        (MonadDOM m, ToJSString val) => HTMLAnchorElement -> val -> m ()
setHref self val = liftDOM (self ^. jss "href" (toJSVal val))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.href Mozilla HTMLAnchorElement.href documentation> 
getHref ::
        (MonadDOM m, FromJSString result) => HTMLAnchorElement -> m result
getHref self = liftDOM ((self ^. js "href") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.hreflang Mozilla HTMLAnchorElement.hreflang documentation> 
setHreflang ::
            (MonadDOM m, ToJSString val) => HTMLAnchorElement -> val -> m ()
setHreflang self val
  = liftDOM (self ^. jss "hreflang" (toJSVal val))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.hreflang Mozilla HTMLAnchorElement.hreflang documentation> 
getHreflang ::
            (MonadDOM m, FromJSString result) => HTMLAnchorElement -> m result
getHreflang self
  = liftDOM ((self ^. js "hreflang") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.name Mozilla HTMLAnchorElement.name documentation> 
setName ::
        (MonadDOM m, ToJSString val) => HTMLAnchorElement -> val -> m ()
setName self val = liftDOM (self ^. jss "name" (toJSVal val))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.name Mozilla HTMLAnchorElement.name documentation> 
getName ::
        (MonadDOM m, FromJSString result) => HTMLAnchorElement -> m result
getName self = liftDOM ((self ^. js "name") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.ping Mozilla HTMLAnchorElement.ping documentation> 
setPing ::
        (MonadDOM m, ToJSString val) => HTMLAnchorElement -> val -> m ()
setPing self val = liftDOM (self ^. jss "ping" (toJSVal val))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.ping Mozilla HTMLAnchorElement.ping documentation> 
getPing ::
        (MonadDOM m, FromJSString result) => HTMLAnchorElement -> m result
getPing self = liftDOM ((self ^. js "ping") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.rel Mozilla HTMLAnchorElement.rel documentation> 
setRel ::
       (MonadDOM m, ToJSString val) => HTMLAnchorElement -> val -> m ()
setRel self val = liftDOM (self ^. jss "rel" (toJSVal val))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.rel Mozilla HTMLAnchorElement.rel documentation> 
getRel ::
       (MonadDOM m, FromJSString result) => HTMLAnchorElement -> m result
getRel self = liftDOM ((self ^. js "rel") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.rev Mozilla HTMLAnchorElement.rev documentation> 
setRev ::
       (MonadDOM m, ToJSString val) => HTMLAnchorElement -> val -> m ()
setRev self val = liftDOM (self ^. jss "rev" (toJSVal val))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.rev Mozilla HTMLAnchorElement.rev documentation> 
getRev ::
       (MonadDOM m, FromJSString result) => HTMLAnchorElement -> m result
getRev self = liftDOM ((self ^. js "rev") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.shape Mozilla HTMLAnchorElement.shape documentation> 
setShape ::
         (MonadDOM m, ToJSString val) => HTMLAnchorElement -> val -> m ()
setShape self val = liftDOM (self ^. jss "shape" (toJSVal val))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.shape Mozilla HTMLAnchorElement.shape documentation> 
getShape ::
         (MonadDOM m, FromJSString result) => HTMLAnchorElement -> m result
getShape self
  = liftDOM ((self ^. js "shape") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.target Mozilla HTMLAnchorElement.target documentation> 
setTarget ::
          (MonadDOM m, ToJSString val) => HTMLAnchorElement -> val -> m ()
setTarget self val = liftDOM (self ^. jss "target" (toJSVal val))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.target Mozilla HTMLAnchorElement.target documentation> 
getTarget ::
          (MonadDOM m, FromJSString result) => HTMLAnchorElement -> m result
getTarget self
  = liftDOM ((self ^. js "target") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.type Mozilla HTMLAnchorElement.type documentation> 
setType ::
        (MonadDOM m, ToJSString val) => HTMLAnchorElement -> val -> m ()
setType self val = liftDOM (self ^. jss "type" (toJSVal val))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.type Mozilla HTMLAnchorElement.type documentation> 
getType ::
        (MonadDOM m, FromJSString result) => HTMLAnchorElement -> m result
getType self = liftDOM ((self ^. js "type") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.hash Mozilla HTMLAnchorElement.hash documentation> 
setHash ::
        (MonadDOM m, ToJSString val) =>
          HTMLAnchorElement -> Maybe val -> m ()
setHash self val = liftDOM (self ^. jss "hash" (toJSVal val))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.hash Mozilla HTMLAnchorElement.hash documentation> 
getHash ::
        (MonadDOM m, FromJSString result) =>
          HTMLAnchorElement -> m (Maybe result)
getHash self = liftDOM ((self ^. js "hash") >>= fromMaybeJSString)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.hash Mozilla HTMLAnchorElement.hash documentation> 
getHashUnsafe ::
              (MonadDOM m, HasCallStack, FromJSString result) =>
                HTMLAnchorElement -> m result
getHashUnsafe self
  = liftDOM
      (((self ^. js "hash") >>= fromMaybeJSString) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.hash Mozilla HTMLAnchorElement.hash documentation> 
getHashUnchecked ::
                 (MonadDOM m, FromJSString result) => HTMLAnchorElement -> m result
getHashUnchecked self
  = liftDOM ((self ^. js "hash") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.host Mozilla HTMLAnchorElement.host documentation> 
setHost ::
        (MonadDOM m, ToJSString val) =>
          HTMLAnchorElement -> Maybe val -> m ()
setHost self val = liftDOM (self ^. jss "host" (toJSVal val))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.host Mozilla HTMLAnchorElement.host documentation> 
getHost ::
        (MonadDOM m, FromJSString result) =>
          HTMLAnchorElement -> m (Maybe result)
getHost self = liftDOM ((self ^. js "host") >>= fromMaybeJSString)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.host Mozilla HTMLAnchorElement.host documentation> 
getHostUnsafe ::
              (MonadDOM m, HasCallStack, FromJSString result) =>
                HTMLAnchorElement -> m result
getHostUnsafe self
  = liftDOM
      (((self ^. js "host") >>= fromMaybeJSString) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.host Mozilla HTMLAnchorElement.host documentation> 
getHostUnchecked ::
                 (MonadDOM m, FromJSString result) => HTMLAnchorElement -> m result
getHostUnchecked self
  = liftDOM ((self ^. js "host") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.hostname Mozilla HTMLAnchorElement.hostname documentation> 
setHostname ::
            (MonadDOM m, ToJSString val) =>
              HTMLAnchorElement -> Maybe val -> m ()
setHostname self val
  = liftDOM (self ^. jss "hostname" (toJSVal val))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.hostname Mozilla HTMLAnchorElement.hostname documentation> 
getHostname ::
            (MonadDOM m, FromJSString result) =>
              HTMLAnchorElement -> m (Maybe result)
getHostname self
  = liftDOM ((self ^. js "hostname") >>= fromMaybeJSString)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.hostname Mozilla HTMLAnchorElement.hostname documentation> 
getHostnameUnsafe ::
                  (MonadDOM m, HasCallStack, FromJSString result) =>
                    HTMLAnchorElement -> m result
getHostnameUnsafe self
  = liftDOM
      (((self ^. js "hostname") >>= fromMaybeJSString) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.hostname Mozilla HTMLAnchorElement.hostname documentation> 
getHostnameUnchecked ::
                     (MonadDOM m, FromJSString result) => HTMLAnchorElement -> m result
getHostnameUnchecked self
  = liftDOM ((self ^. js "hostname") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.pathname Mozilla HTMLAnchorElement.pathname documentation> 
setPathname ::
            (MonadDOM m, ToJSString val) =>
              HTMLAnchorElement -> Maybe val -> m ()
setPathname self val
  = liftDOM (self ^. jss "pathname" (toJSVal val))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.pathname Mozilla HTMLAnchorElement.pathname documentation> 
getPathname ::
            (MonadDOM m, FromJSString result) =>
              HTMLAnchorElement -> m (Maybe result)
getPathname self
  = liftDOM ((self ^. js "pathname") >>= fromMaybeJSString)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.pathname Mozilla HTMLAnchorElement.pathname documentation> 
getPathnameUnsafe ::
                  (MonadDOM m, HasCallStack, FromJSString result) =>
                    HTMLAnchorElement -> m result
getPathnameUnsafe self
  = liftDOM
      (((self ^. js "pathname") >>= fromMaybeJSString) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.pathname Mozilla HTMLAnchorElement.pathname documentation> 
getPathnameUnchecked ::
                     (MonadDOM m, FromJSString result) => HTMLAnchorElement -> m result
getPathnameUnchecked self
  = liftDOM ((self ^. js "pathname") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.port Mozilla HTMLAnchorElement.port documentation> 
setPort ::
        (MonadDOM m, ToJSString val) =>
          HTMLAnchorElement -> Maybe val -> m ()
setPort self val = liftDOM (self ^. jss "port" (toJSVal val))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.port Mozilla HTMLAnchorElement.port documentation> 
getPort ::
        (MonadDOM m, FromJSString result) =>
          HTMLAnchorElement -> m (Maybe result)
getPort self = liftDOM ((self ^. js "port") >>= fromMaybeJSString)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.port Mozilla HTMLAnchorElement.port documentation> 
getPortUnsafe ::
              (MonadDOM m, HasCallStack, FromJSString result) =>
                HTMLAnchorElement -> m result
getPortUnsafe self
  = liftDOM
      (((self ^. js "port") >>= fromMaybeJSString) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.port Mozilla HTMLAnchorElement.port documentation> 
getPortUnchecked ::
                 (MonadDOM m, FromJSString result) => HTMLAnchorElement -> m result
getPortUnchecked self
  = liftDOM ((self ^. js "port") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.protocol Mozilla HTMLAnchorElement.protocol documentation> 
setProtocol ::
            (MonadDOM m, ToJSString val) =>
              HTMLAnchorElement -> Maybe val -> m ()
setProtocol self val
  = liftDOM (self ^. jss "protocol" (toJSVal val))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.protocol Mozilla HTMLAnchorElement.protocol documentation> 
getProtocol ::
            (MonadDOM m, FromJSString result) =>
              HTMLAnchorElement -> m (Maybe result)
getProtocol self
  = liftDOM ((self ^. js "protocol") >>= fromMaybeJSString)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.protocol Mozilla HTMLAnchorElement.protocol documentation> 
getProtocolUnsafe ::
                  (MonadDOM m, HasCallStack, FromJSString result) =>
                    HTMLAnchorElement -> m result
getProtocolUnsafe self
  = liftDOM
      (((self ^. js "protocol") >>= fromMaybeJSString) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.protocol Mozilla HTMLAnchorElement.protocol documentation> 
getProtocolUnchecked ::
                     (MonadDOM m, FromJSString result) => HTMLAnchorElement -> m result
getProtocolUnchecked self
  = liftDOM ((self ^. js "protocol") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.search Mozilla HTMLAnchorElement.search documentation> 
setSearch ::
          (MonadDOM m, ToJSString val) =>
            HTMLAnchorElement -> Maybe val -> m ()
setSearch self val = liftDOM (self ^. jss "search" (toJSVal val))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.search Mozilla HTMLAnchorElement.search documentation> 
getSearch ::
          (MonadDOM m, FromJSString result) =>
            HTMLAnchorElement -> m (Maybe result)
getSearch self
  = liftDOM ((self ^. js "search") >>= fromMaybeJSString)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.search Mozilla HTMLAnchorElement.search documentation> 
getSearchUnsafe ::
                (MonadDOM m, HasCallStack, FromJSString result) =>
                  HTMLAnchorElement -> m result
getSearchUnsafe self
  = liftDOM
      (((self ^. js "search") >>= fromMaybeJSString) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.search Mozilla HTMLAnchorElement.search documentation> 
getSearchUnchecked ::
                   (MonadDOM m, FromJSString result) => HTMLAnchorElement -> m result
getSearchUnchecked self
  = liftDOM ((self ^. js "search") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.origin Mozilla HTMLAnchorElement.origin documentation> 
getOrigin ::
          (MonadDOM m, FromJSString result) =>
            HTMLAnchorElement -> m (Maybe result)
getOrigin self
  = liftDOM ((self ^. js "origin") >>= fromMaybeJSString)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.origin Mozilla HTMLAnchorElement.origin documentation> 
getOriginUnsafe ::
                (MonadDOM m, HasCallStack, FromJSString result) =>
                  HTMLAnchorElement -> m result
getOriginUnsafe self
  = liftDOM
      (((self ^. js "origin") >>= fromMaybeJSString) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.origin Mozilla HTMLAnchorElement.origin documentation> 
getOriginUnchecked ::
                   (MonadDOM m, FromJSString result) => HTMLAnchorElement -> m result
getOriginUnchecked self
  = liftDOM ((self ^. js "origin") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.text Mozilla HTMLAnchorElement.text documentation> 
setText ::
        (MonadDOM m, ToJSString val) => HTMLAnchorElement -> val -> m ()
setText self val = liftDOM (self ^. jss "text" (toJSVal val))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.text Mozilla HTMLAnchorElement.text documentation> 
getText ::
        (MonadDOM m, FromJSString result) => HTMLAnchorElement -> m result
getText self = liftDOM ((self ^. js "text") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.relList Mozilla HTMLAnchorElement.relList documentation> 
getRelList ::
           (MonadDOM m) => HTMLAnchorElement -> m (Maybe DOMTokenList)
getRelList self = liftDOM ((self ^. js "relList") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.relList Mozilla HTMLAnchorElement.relList documentation> 
getRelListUnsafe ::
                 (MonadDOM m, HasCallStack) => HTMLAnchorElement -> m DOMTokenList
getRelListUnsafe self
  = liftDOM
      (((self ^. js "relList") >>= fromJSVal) >>=
         maybe (Prelude.error "Nothing to return") return)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement.relList Mozilla HTMLAnchorElement.relList documentation> 
getRelListUnchecked ::
                    (MonadDOM m) => HTMLAnchorElement -> m DOMTokenList
getRelListUnchecked self
  = liftDOM ((self ^. js "relList") >>= fromJSValUnchecked)
