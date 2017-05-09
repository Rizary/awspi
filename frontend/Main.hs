{-# LANGUAGE CPP, OverloadedStrings, RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds, PatternSynonyms, LambdaCase #-}

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
import qualified GI.Gtk.Functions as GI (main, init)
import GI.Gtk.Objects.Action (onActionActivate, actionNew)
import GI.Gtk.Functions (mainQuit)
import GI.Gtk.Objects.ActionGroup
       (actionGroupAddActionWithAccel, actionGroupAddAction,
        actionGroupNew)
import GI.Gtk.Objects.UIManager
       (uIManagerGetWidget, uIManagerInsertActionGroup,
        uIManagerAddUiFromString, uIManagerNew)
import GI.Gtk.Objects.Window (windowNew)
import GI.Gtk.Objects.Widget
       (widgetShowAll, setWidgetHeightRequest, setWidgetWidthRequest,
        onWidgetDestroy)
import GI.Gtk.Objects.TextView (textViewNew)
import GI.Gtk.Objects.Box (boxNew, boxPackStart, setBoxHomogeneous)
import GI.Gtk.Objects.Container (containerAdd)
import GI.Gtk.Enums (Orientation (..), WindowType(..))
import GI.Gtk
       (windowSetPosition, windowSetDefaultSize, windowNew,
        scrolledWindowNew, noAdjustment, containerAdd,
        WindowType(..), WindowPosition(..), widgetDestroy,
        widgetGetToplevel, widgetShowAll, onWidgetDestroy,
        mainQuit)
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSBase
       (JSValueRef, JSContextRef)
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSStringRef
       (jsstringgetcharactersptr, jsstringgetlength, jsstringrelease)
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSValueRef
       (jsvaluetostringcopy)
import GI.WebKit2
import qualified Data.Text as T (length)
import Language.Javascript.JSaddle.WebKitGTK
--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------

main :: IO()
main = withMenu mainWk

mainWk :: JSM ()
mainWk = mainWidget bodyElement

headElement :: MonadWidget t m => m ()
headElement = do
  el "title" $ text "AWS Simulator"
  styleSheet "static/stylesheet/simple.css"
  where
    styleSheet link = elAttr "link" (Map.fromList [
          ("rel", "stylesheet")
        , ("type", "text/css")
        , ("href", link)
      ]) $ return ()

bodyElement :: MonadWidget t m => m ()
bodyElement = elClass "div" "mainBody" $ do
  el "h1" $ text "Welcome to AWS Simulation Tools"
  el "div" $ do
    el "p" $ text "This application is used for:"
    el "ul" $ do
      el "li" $ text "Company whose planning to move to the Cloud"
      el "li" $ text "Developer whom trying to create AWS based Server"
    el "p" $ do
      text "Please go to this link "
      elAttr "a" linkAttrs $ text "Rizilab"
  el "br" blanks
  elAttr "object" svgAttrs $ text "AWS Logo"
  el "br" blanks
  bottomBar
  rightSideBar
  anotherTexInput
  anotherTexInput2
  readingValue
  testTextArea
  testCheckBox
  dropDown
  rangeWidget
  
  


linkAttrs :: Map.Map T.Text T.Text
linkAttrs = ("target" =: "_blank") <> ("href" =: "http://rizilab.com")

colorAttrs :: Bool -> Map.Map T.Text T.Text
colorAttrs b = "style" =: ("color: " <> color b)
  where
    color True = "red"
    color _    = "green"

svgAttrs :: Map.Map T.Text T.Text
svgAttrs = ("type" =: "image/svg+xml") <>
           ("data" =: "static/svg/ec2.svg") <>
           ("class" =: "logo")

------
-- Bottom and Right Bar
-----
bottomBar :: MonadWidget t m => m ()
bottomBar = elClass "div" "theButton" $ do
    evClick <- button "Change Color"
    dynBool <- toggle False evClick
    let dynAttrs = colorAttrs <$> dynBool
    elDynAttr "h1" dynAttrs $ text "Changing color"
    return ()

rightSideBar :: MonadWidget t m => m ()
rightSideBar = do
  rec
    el "h2" $ text "Counter as fold"
    numbs <- foldDyn (+) (0 :: Int) $ leftmost [1 <$ evIncr, -1 <$ evDecr]
    el "div" $ display numbs
    evIncr <- button "Increment"
    evDecr <- button "Decrement"
  return ()

anotherTexInput :: MonadWidget t m => m ()
anotherTexInput = el "div" $ do
  el "h2" $ text "Simple text input"
  ti <- textInput def
  dynText $ value ti

anotherTexInput2 :: MonadWidget t m => m ()
anotherTexInput2 = do
  el "h2" $ text "Another text sample"

  el "h4" $ text "Mak length 14"
  t1 <- textInput $ def & attributes .~ constDyn ("maxlength" =: "14")
  dynText $ _textInput_value t1

  el "h4" $ text "Initial Value"
  t2 <- textInput $ def & textInputConfig_initialValue .~ "input"
  dynText $ _textInput_value t2

  el "h4" $ text "Input Hint"
  t3 <- textInput $
        def & attributes .~ constDyn("placeholder" =: "type something")
  dynText $ _textInput_value t3

  el "h4" $ text "Password"
  t4 <- textInput $ def & textInputConfig_inputType .~ "password"
  dynText $ _textInput_value t4

  el "h4" $ text "Multiple Attributes: Hint + Max Length"
  t5 <- textInput $ def & attributes .~ constDyn ("placeholder" =: "Max 6 chars"
        <> "maxlength" =: "6")
  dynText $ _textInput_value t5

  el "h4" $ text "Numeric Field with initial value"
  t6 <- textInput $ def & textInputConfig_inputType .~ "number"
                        & textInputConfig_initialValue .~ "0"
  dynText $ _textInput_value t6
  return ()

readingValue :: MonadWidget t m => m ()
readingValue = do
  el "h2" $ text " Text Input - Read Value on Button Click"
  ti <- textInput def
  evClick <- button "Read Value"
  el "br" blank
  text "Contents of TextInput on last click: "
  let evText = tagPromptlyDyn (value ti) evClick
  (holdDyn "" evText) >>= dynText

  el "h2" $ text "Text Input - Read Value on 'Enter'"
  ti <- textInput def
  el "br" blank
  text "Contents of TextInput after 'Enter': "
  let evEnter = keypress Enter ti
  let evText = tagPromptlyDyn (value ti) evEnter
  dynText =<< holdDyn "" evText

  el "h1" $ text "Write into TextInput Widget"
  t1 <- textInput def
  evCopy <- button " >>> "
  let evText = tagPromptlyDyn (value t1) evCopy
  rec
    t2 <- textInput $ def & setValue .~ (leftmost [ evText, "" <$ evReset])
    evReset <- button "Reset"
  return ()

testTextArea :: MonadWidget t m => m ()
testTextArea = do
  el "h2" $ text "RGB Viewer"
  el "div" $ text "Enter RGB component values as numbers between 0 and 255"
  dfsRed <- labledBox "Red: "
  dfsGreen <- labledBox "Green: "
  dfsBlue <- labledBox "Blue: "
  textArea $
    def & attributes .~ (styleMap <$> value  dfsRed <*> value dfsGreen <*> value dfsBlue)
  return ()

labledBox :: MonadWidget t m => T.Text -> m (TextInput t)
labledBox lbl = el "div" $ do
  text lbl
  textInput $ def & textInputConfig_inputType .~ "number"
                  & textInputConfig_initialValue .~ "0"

styleMap :: T.Text -> T.Text -> T.Text -> Map.Map T.Text T.Text
styleMap r g b = "style" =: mconcat ["background-color: rgb(",r,",", g, ",", b, ")"]

testCheckBox :: MonadWidget t m => m ()
testCheckBox = el "div" $ do
  el "h2" $ text "Checkbox (Out of the box)"
  cb <- checkbox True def
  text "Click me"
  el "p" blank
  let dynState = checkedState <$> value cb
  dynText dynState

  el "h2" $ text "Chekcbox - User Friendly"
  cbf <- el "label" $ do
    cbf1 <- checkbox True def
    text "Click Me"
    return cbf1
  el "p" blank
  let dynState = checkedState <$> value cbf
  dynText dynState

checkedState :: Bool -> T.Text
checkedState True = "Checkbox is checked"
checkedState _    = "Checkbox is not checked"

dropDown :: MonadWidget t m => m ()
dropDown = do
  el "h2" $ text "Dropdown"
  text "Select country "
  dd <- dropdown 2 (constDyn countries) def
  el "p" blank
  let selItem = result <$> value dd
  dynText selItem

countries :: Map.Map Int T.Text
countries = Map.fromList [(1,"France"), (2, "Switzerland"), (3, "Germany"), (4, "Italy"), (5, "USA")]

result :: Int -> T.Text
result key = "You selected: " <> Maybe.fromJust (Map.lookup key countries)

rangeWidget :: MonadWidget t m => m ()
rangeWidget = do
  el "h2" $ text "Range Input"
  rg <- rangeInput def
  el "p" blank
  display $ _rangeInput_value rg

  el "h2" $ text "Range input with min"
  rg1 <- rangeInput $ def & attributes .~ constDyn ("min" =: "-100")
  el "p" blank
  display $ _rangeInput_value rg1
  return ()

  el "h2" $ text "Range input step 10"
  rg2 <- rangeInput $ def & attributes .~ constDyn
    ("min" =: "-100" <> "max" =: "100" <> "value" =: "0" <> "step" =: "10" <> "list" =: "powers")
  elAttr "datalist" ("id" =: "powers") $ do
    elAttr "option" ("value" =: "0") blank
    elAttr "option" ("value" =: "-30") blank
    elAttr "option" ("value" =: "50") blank
  el "p" blank
  display $ _rangeInput_value rg2
  return ()

  
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

blanks :: forall m. Monad m => m ()
blanks = return ()

-- gi-gtk-examples

uiDef =
  "<ui>\
  \  <menubar>\
  \    <menu name=\"File\" action=\"FileAction\">\
  \      <menuitem name=\"New\" action=\"NewAction\" />\
  \      <menuitem name=\"Open\" action=\"OpenAction\" />\
  \      <menuitem name=\"Save\" action=\"SaveAction\" />\
  \      <menuitem name=\"SaveAs\" action=\"SaveAsAction\" />\
  \      <separator/>\
  \      <menuitem name=\"Exit\" action=\"ExitAction\"/>\
  \      <placeholder name=\"FileMenuAdditions\" />\
  \    </menu>\
  \    <menu name=\"Edit\" action=\"EditAction\">\
  \      <menuitem name=\"Cut\" action=\"CutAction\"/>\
  \      <menuitem name=\"Copy\" action=\"CopyAction\"/>\
  \      <menuitem name=\"Paste\" action=\"PasteAction\"/>\
  \    </menu>\
  \  </menubar>\
  \  <toolbar>\
  \    <placeholder name=\"FileToolItems\">\
  \      <separator/>\
  \      <toolitem name=\"New\" action=\"NewAction\"/>\
  \      <toolitem name=\"Open\" action=\"OpenAction\"/>\
  \      <toolitem name=\"Save\" action=\"SaveAction\"/>\
  \      <separator/>\
  \    </placeholder>\
  \    <placeholder name=\"EditToolItems\">\
  \      <separator/>\
  \      <toolitem name=\"Cut\" action=\"CutAction\"/>\
  \      <toolitem name=\"Copy\" action=\"CopyAction\"/>\
  \      <toolitem name=\"Paste\" action=\"PasteAction\"/>\
  \      <separator/>\
  \    </placeholder>\
  \  </toolbar>\
  \</ui>" :: Text

withMenu :: JSM () -> IO ()
withMenu = do
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
#ifdef mingw32_HOST_OS
installQuitHandler wv = return () -- TODO: Maybe figure something out here for Windows users.
#else
installQuitHandler wv = void $ installHandler keyboardSignal (Catch (quitWebView wv)) Nothing
#endif
