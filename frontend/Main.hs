{-# LANGUAGE OverloadedStrings, RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds, PatternSynonyms, LambdaCase #-}

import Prelude hiding (mapM, mapM_, all, sequence)

import           Control.Monad hiding (mapM, mapM_, forM, forM_, sequence)
import           Control.Monad.Fix
import           Data.Map (Map)
import qualified Data.Map                  as Map
import           Data.Foldable
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text                 as T
import qualified Data.Maybe                as Maybe

import GHCJS.DOM.Types (JSM)

import Reflex
import Reflex.Dom.Core
import Data.Text.Encoding (encodeUtf8)
import Debug.Trace (trace)

-- This import is taken from gi-gtk-examples
import System.Directory (getCurrentDirectory)
import System.Posix.Signals (installHandler, keyboardSignal, Handler(Catch))

  _ <- GI.init Nothing

  -- Create the menus
  fileAct <- actionNew "FileAction" (Just (__"File")) Nothing Nothing
  editAct <- actionNew "EditAction" (Just (__"Edit")) Nothing Nothing

  -- Create menu items
  newAct <- actionNew "NewAction" (Just (__"New"))
            (Just (__"Clear the spreadsheet area."))
            (Just "_New")
  onActionActivate newAct $ putStrLn "New activated."
  openAct <- actionNew "OpenAction" (Just (__"Open"))
            (Just (__"Open an existing spreadsheet."))
            (Just "_Open")
  onActionActivate openAct $ putStrLn "Open activated."
  saveAct <- actionNew "SaveAction" (Just (__"Save"))
            (Just (__"Save the current spreadsheet."))
            (Just "_Save")
  onActionActivate saveAct $ putStrLn "Save activated."
  saveAsAct <- actionNew "SaveAsAction" (Just (__"SaveAs"))
            (Just (__"Save spreadsheet under new name."))
            (Just "Save_As")
  onActionActivate saveAsAct $ putStrLn "SaveAs activated."
  exitAct <- actionNew "ExitAction" (Just (__"Exit"))
            (Just (__"Exit this application."))
            (Just "_Quit")
  onActionActivate exitAct mainQuit
  cutAct <- actionNew "CutAction" (Just (__"Cut"))
            (Just (__"Cut out the current selection."))
            (Just "Cu_t")
  onActionActivate cutAct $ putStrLn "Cut activated."
  copyAct <- actionNew "CopyAction" (Just (__"Copy"))
            (Just (__"Copy the current selection."))
            (Just "_Copy")
  onActionActivate copyAct $ putStrLn "Copy activated."
  pasteAct <- actionNew "PasteAction" (Just (__"Paste"))
            (Just (__"Paste the current selection."))
            (Just "_Paste")
  onActionActivate pasteAct $ putStrLn "Paste activated."

  standardGroup <- actionGroupNew ("standard"::Text)
  mapM_ (actionGroupAddAction standardGroup) [fileAct, editAct]
  mapM_ (\act -> actionGroupAddActionWithAccel standardGroup act (Nothing::Maybe Text))
    [newAct, openAct, saveAct, saveAsAct, exitAct, cutAct, copyAct, pasteAct]

  ui <- uIManagerNew
  mid <- uIManagerAddUiFromString ui uiDef (fromIntegral $ T.length uiDef)
  uIManagerInsertActionGroup ui standardGroup 0

  win <- windowNew WindowTypeToplevel

  windowSetDefaultSize win 900 600
  windowSetPosition win WindowPositionCenter
  scrollWin <- scrolledWindowNew noAdjustment noAdjustment
  contentManager <- userContentManagerNew
  webView <- webViewNewWithUserContentManager contentManager
  settings <- webViewGetSettings webView
  setSettingsEnableDeveloperExtras settings True
  setSettingsEnableJavascript settings True
  setSettingsEnableWriteConsoleMessagesToStdout settings True
  webViewSetSettings webView settings
  win `containerAdd` scrollWin
  scrollWin `containerAdd` webView
  _ <- onWidgetDestroy win mainQuit
  setWidgetWidthRequest win 200
  setWidgetHeightRequest win 100
  menuBar <- uIManagerGetWidget ui "/ui/menubar"
  toolBar <- uIManagerGetWidget ui "/ui/toolbar"
  pwd <- getCurrentDirectory
  void . onWebViewLoadChanged webView $ \case
      LoadEventFinished -> runInWebView main webView
      _                 -> return ()
  webViewLoadHtml  webView "" . Just $ "file://" <> T.pack pwd <> "/"
  installQuitHandler webView

  edit <- textViewNew
  vBox <- boxNew OrientationVertical 0
  setBoxHomogeneous vBox False
  boxPackStart vBox menuBar False False 0
  boxPackStart vBox toolBar False False 0
  boxPackStart vBox edit True True 0

  widgetShowAll win

  GI.main


__ :: Text -> Text
__ = id


quitWebView :: WebView -> IO ()
quitWebView wv = postGUIAsync $ do w <- widgetGetToplevel wv --TODO: Shouldn't this be postGUISync?
                                   widgetDestroy w

installQuitHandler :: WebView -> IO ()
installQuitHandler wv = void $ installHandler keyboardSignal (Catch (quitWebView wv)) Nothing

postGUIAsync :: IO () -> IO ()
postGUIAsync action =
  void . idleAdd PRIORITY_DEFAULT $ action >> return False