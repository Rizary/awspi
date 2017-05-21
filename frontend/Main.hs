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
import           Control.Monad.Ref (MonadRef(..))

import GHCJS.DOM.Types (JSM, File, Element(..))

import Reflex
import Reflex.Dom.Core
import Reflex.Dom.Contrib.Widgets.DynamicList
import Reflex.Dom.Contrib.Widgets.Common
import Reflex.Dom.Contrib.Widgets.ButtonGroup
import Data.Text.Encoding (encodeUtf8)
import Debug.Trace (trace)
import Language.Javascript.JSaddle.WKWebView (run, runFile)
import Language.Javascript.JSaddle.Types (MonadJSM, liftJSM)
-- import Frontend.Types
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

bodyElement :: MonadWidget t m => m ()
bodyElement = elClass "div" "container" $ do
  rec
-- Menu utama pemilihan jasa AWS yang paling umum digunakan
    elClass "div" "middleMenu" $ do
      rec
        ec2 <- serviceButton "Amazon EC2" EC2 ec2dynAttrs testText1
        s3 <- serviceButton "Amazon S3" S3 s3dynAttrs testText1
        r53 <- serviceButton "Route 53" R53 r53dynAttrs testText1
        cf <- serviceButton "CloudFront" CF cfdynAttrs testText1
        rds <- serviceButton "Amazon RDS" RDS rdsdynAttrs testText1
        db <- serviceButton "DynamoDB" DB dbddynAttrs testText1
        ec <- serviceButton "Elastic Cache" EC ecdynAttrs testText1
        cw <- serviceButton "CloudWatch" CW cwdynAttrs testText1
        vpc <- serviceButton "Amazon VPC" VPC vpcdynAttrs testText1
        ls <- serviceButton "Amazon Lightsail" LS lsdynAttrs testText1
        ec2dynBool <- toggle False ec2
	s3dynBool  <- toggle False s3
	r53dynBool <- toggle False r53
	cfdynBool  <- toggle False cf
	rdsdynBool <- toggle False rds
	dbdynBool  <- toggle False db
	ecdynBool  <- toggle False ec
	cwdynBool  <- toggle False cw
	vpcdynBool <- toggle False vpc
	lsdynBool  <- toggle False ls
	
        let ec2dynAttrs = labelAttrs <$> ec2dynBool
	    s3dynAttrs = labelAttrs <$> s3dynBool
	    r53dynAttrs = labelAttrs <$> r53dynBool
	    cfdynAttrs = labelAttrs <$> cfdynBool
	    rdsdynAttrs = labelAttrs <$> rdsdynBool
	    dbddynAttrs = labelAttrs <$> dbdynBool
	    ecdynAttrs = labelAttrs <$> ecdynBool
	    cwdynAttrs = labelAttrs <$> cwdynBool
	    vpcdynAttrs = labelAttrs <$> vpcdynBool
	    lsdynAttrs = labelAttrs <$> lsdynBool
	    
      return ()
-- Menu Properti dari masing-masing jasa AWS yang sedang aktif
    elClass "div" "rightMenu" $ do
     el "h2" $ text "Properties: "
      -- elDynClass "div" awsPropAttrs $ do
        


-- Menu summary berupa hasil perhitungan final dari semua jasa AWS
    elClass "div" "summaryMenu" $ do
     el "h2" $ text "Summary: "
  return ()
  


-----
-- Right Bar
-----
awsPropAttrs :: Map.Map T.Text T.Text
awsPropAttrs = undefined


serviceProperties = undefined

simulateResults = undefined


-----
-- Middle Menu (Icon Selector)
-----

-----
-- Summary Bar (Icon Selector)
-----

-----
-- AWS Related Function
-----
imgAttrs :: AwsIcon -> Map.Map T.Text T.Text
imgAttrs awi = ("src" =: (iconName awi)) <>
               ("width" =: "81") <>
	       ("height" =: "80")

idAttrs :: AwsIcon -> Map.Map T.Text T.Text
idAttrs awi = ("id" =: T.pack (show awi))


iconName :: AwsIcon -> T.Text
iconName awi =
  case awi of
    CF  -> "static/svg/AmazonCloudFront.png"
    CW  -> "static/svg/AmazonCloudWatch.png"
    DB  -> "static/svg/AmazonDynamoDB.png"
    EC2 -> "static/svg/AmazonEC2.png"
    EC  -> "static/svg/AmazonElasticCache.png"
    LS  -> "static/svg/AmazonLightsail.png"
    RDS -> "static/svg/AmazonRDS.png"
    R53 -> "static/svg/AmazonRoute53.png"
    S3  -> "static/svg/AmazonS3.png"
    VPC  -> "static/svg/AmazonVPC.png"
    _ -> ""

data AwsIcon = CF | CW | DB | EC2 | EC | LS | RDS | R53 | S3 | VPC
  deriving (Eq, Show, Ord)


calculatedPrice = undefined

blanks :: forall m. Monad m => m ()
blanks = return ()


labelAttrs :: Bool -> Map.Map T.Text T.Text
labelAttrs b = "style" =: ("background-color: " <> color b)
  where
    color True = "hsla(171,100%,84%,1)"
    color _    = "transparent"

serviceButton :: (MonadWidget t m) => T.Text
                                   -> AwsIcon
				   -> Dynamic t (Map.Map T.Text T.Text)
				   -> Dynamic t T.Text
				   -> m (Event t AwsIcon)
serviceButton label icon dynAttrs dynCost = do
    (ev1,_) <- elDynAttr' "label" ((constDyn $ idAttrs icon) <> dynAttrs) $ do
            elAttr' "img" (imgAttrs icon) $ text ""
            el "h2" $ text label
	    let text1 = constDyn "Monthly Cost: $ "
            el "h4" $ dynText $ text1 <> dynCost
    return $ icon <$ (domEvent Click ev1)

testText1 :: Reflex t => Dynamic t T.Text
testText1 = constDyn "0"




{-|
el "label" $ do
        elAttr "img" (imgAttrs EC2) $ text ""
        el "h2" $ text "Amazon EC2"
	el "br" blanks
        el "h4" $ text "Monthly Cost: $ "
      el "label" $ do
        elAttr "img" (imgAttrs S3) $ text ""
        el "h2" $ text "Amazon S3"
	el "br" blanks
        el "h4" $ text "Monthly Cost: $ "
      el "label" $ do
        elAttr "img" (imgAttrs R53) $ text ""
        el "h2" $ text "Route 53"
	el "br" blanks
        el "h4" $ text "Monthly Cost: $ "
      el "label" $ do
        elAttr "img" (imgAttrs CF) $ text ""
        el "h2" $ text "CloudFront"
	el "br" blanks
        el "h4" $ text "Monthly Cost: $ "
      el "label" $ do
        elAttr "img" (imgAttrs RDS) $ text ""
        el "h2" $ text "Amazon RDS"
	el "br" blanks
        el "h4" $ text "Monthly Cost: $ "
      el "label" $ do
        elAttr "img" (imgAttrs DB) $ text ""
        el "h2" $ text "DynamoDB"
	el "br" blanks
        el "h4" $ text "Monthly Cost: $ "
      el "label" $ do
        elAttr "img" (imgAttrs EC) $ text ""
        el "h2" $ text "Elastic Cache"
	el "br" blanks
        el "h4" $ text "Monthly Cost: $ "
      el "label" $ do
        elAttr "img" (imgAttrs CW) $ text ""
        el "h2" $ text "CloudWatch"
	el "br" blanks
        el "h4" $ text "Monthly Cost: $ "
      el "label" $ do
        elAttr "img" (imgAttrs VPC)$ text ""
        el "h2" $ text "Amazon VPC"
	el "br" blanks
        el "h4" $ text "Monthly Cost: $ "
      el "label" $ do
        elAttr "img" (imgAttrs LS) $ text ""
        el "h2" $ text "Amazon LightSail"
	el "br" blanks
        el "h4" $ text "Monthly Cost: $ "
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
  radioButton
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
radioButton :: MonadWidget t m => m ()
radioButton = do
  el "h2" $ text "Radio Buttons from Contrib Library"
  rec
    rbs :: HtmlWidget t (Maybe Selection) <-
      radioGroup
          (constDyn "size")
	  (constDyn [(Small, "small"), (Medium, "Medium"), (Large, "LARGE")])
          WidgetConfig { _widgetConfig_initialValue = Nothing
                       , _widgetConfig_setValue     = never
                       , _widgetConfig_attributes   = constDyn mempty
                       }
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
-}