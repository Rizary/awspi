{-# LANGUAGE OverloadedStrings, RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds #-}

import Prelude hiding (mapM, mapM_, all, sequence)

import           Control.Monad hiding (mapM, mapM_, forM, forM_, sequence)
import           Control.Monad.Fix
import           Data.Map (Map)
import qualified Data.Map                  as Map
import           Data.Foldable
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text                 as T

import GHCJS.DOM.Types (JSM)

import Reflex
import Reflex.Dom.Core
import Data.Text.Encoding (encodeUtf8)
import Debug.Trace (trace)
import Language.Javascript.JSaddle.WKWebView (runFile)

--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------

main :: IO()
main = runFile "css/simple.css" "" mainWk

mainWk :: JSM ()
mainWk = mainWidgetWithHead headElement bodyElement

headElement :: MonadWidget t m => m ()
headElement = do
  el "title" $ text "AWS Simulator"
  styleSheet "css/simple.css"
 where
    styleSheet link = elAttr "link" (Map.fromList [
        ("rel", "stylesheet")
      , ("type", "text/css")
      , ("href", link)
     ]) $ return ()

bodyElement :: MonadWidget t m => m ()
bodyElement = el "div" $ do
  el "h1" $ text "This title should be green"
  return ()

------
-- Bottom and Right Bar
-----

bottomBar = undefined

rightSideBar = undefined

-----
-- Bottom Menu
-----

iconServiceGroups = undefined

eachGroupMembers = undefined

otherIconTools = undefined

simulateRealTime = undefined





-----
-- Right Menu
-----

serviceProperties = undefined

simulateResults = undefined





-----
-- Middle Menu (SVG)
-----








-----
-- AWS Related Function
-----

calculatedPrice = undefined









