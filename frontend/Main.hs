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
import qualified Data.Maybe                as Maybe

import GHCJS.DOM.Types (JSM)

import Reflex
import Reflex.Dom.Core
import Reflex.Dom.Contrib.Widgets.DynamicList
import Reflex.Dom.Contrib.Widgets.Common
import Reflex.Dom.Contrib.Widgets.ButtonGroup
import Data.Text.Encoding (encodeUtf8)
import Debug.Trace (trace)
import Language.Javascript.JSaddle.WKWebView (run, runFile)
import Frontend.Types
--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------

main :: IO ()
main = runFile "index.html" "" mainWk

mainWk :: JSM ()
mainWk = mainWidgetWithHead headElement bodyElement

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

test = Node "Andika"

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
  -- radioButton
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

{-| radioButton :: MonadWidget t m => m ()
radioButton = do
  el "h2" $ text "Radio Buttons from Contrib Library"
  rec
    rbs :: HtmlWidget t (Maybe Selection) <-
      radioGroup
          (constDyn "size")
	  (constDyn [(Small, "small"), (Medium, "Medium"), (Large, "LARGE")])
          WidgetConfig { _widgetConfig_initialValue = Nothing
                       , _widgetConfig_setValue     = never
                       , _widgetConfig_attributes   = constDyn mempty}
    text "Result: "
    display (translate <$> _hwidget_value rbs)
  return ()
data Selection = Small | Medium | Large 
  deriving Eq
translate :: Maybe Selection -> T.Text
translate Nothing = "0"
translate (Just Small) = "10"
translate (Just Medium) = "50"
translate (Just Large) = "800"
-}

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







