{-# LANGUAGE OverloadedStrings, RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds #-}

import Prelude hiding (mapM, mapM_, all, sequence)

import           Control.Monad hiding (mapM, mapM_, forM, forM_, sequence)
import           Control.Monad.Fix
import           Control.Applicative (liftA, liftA2)
import           Data.Map (Map)
import qualified Data.Map                  as Map
import           Data.Foldable
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text                 as T
import qualified Data.Text.Read            as TR
import qualified Data.Maybe                as Maybe
import           Control.Monad.Ref (MonadRef(..))
import           Numeric (showFFloat)

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
    midM <- elClass "div" "middleMenu" $ do
             rec
              ec2 <- serviceButton "Amazon EC2" EC2 ec2dynAttrs (theDynCost rightM 1)
              s3  <- serviceButton "Amazon S3" S3 s3dynAttrs (theDynCost rightM 2)
              r53 <- serviceButton "Route 53" R53 r53dynAttrs (theDynCost rightM 3)
              cf  <- serviceButton "CloudFront" CF cfdynAttrs (theDynCost rightM 4)
              rds <- serviceButton "Amazon RDS" RDS rdsdynAttrs (theDynCost rightM 5)
              db  <- serviceButton "DynamoDB" DB dbddynAttrs (theDynCost rightM 6)
              ec  <- serviceButton "Elastic Cache" EC ecdynAttrs (theDynCost rightM 7)
              cw  <- serviceButton "CloudWatch" CW cwdynAttrs (theDynCost rightM 8)
              vpc <- serviceButton "Amazon VPC" VPC vpcdynAttrs (theDynCost rightM 9)
              ls  <- serviceButton "Amazon Lightsail" LS lsdynAttrs (theDynCost rightM 10)

              let 
	          ec2dynAttrs = labelAttrs <$> curSelect dynSelect EC2
	          s3dynAttrs  = labelAttrs <$> curSelect dynSelect S3
	          r53dynAttrs = labelAttrs <$> curSelect dynSelect R53
	          cfdynAttrs  = labelAttrs <$> curSelect dynSelect CF
	          rdsdynAttrs = labelAttrs <$> curSelect dynSelect RDS
	          dbddynAttrs = labelAttrs <$> curSelect dynSelect DB
	          ecdynAttrs  = labelAttrs <$> curSelect dynSelect EC
	          cwdynAttrs  = labelAttrs <$> curSelect dynSelect CW
	          vpcdynAttrs = labelAttrs <$> curSelect dynSelect VPC
	          lsdynAttrs  = labelAttrs <$> curSelect dynSelect LS
              dynSelect <- holdDyn Nothing $ Just <$> leftmost [ec2, s3, r53, cf, rds, db, ec, cw, vpc, ls]
             return dynSelect
	     
-- Menu Properti dari masing-masing jasa AWS yang sedang aktif
    rightM <- elClass "div" "rightMenu" $ do
               el "h2" $ text "Properties: "
               rec
                ec2Prop <- ec2Properties ec2dynAttrs
                s3Prop  <- s3Properties s3dynAttrs
                r53Prop  <- r53Properties r53dynAttrs
                cfProp  <- cfProperties cfdynAttrs
                rdsProp  <- rdsProperties rdsdynAttrs
                dbProp  <- dbProperties dbdynAttrs
                ecProp  <- ecProperties ecdynAttrs
                cwProp  <- cwProperties cwdynAttrs
                vpcProp  <- vpcProperties vpcdynAttrs
                lsProp  <- lsProperties lsdynAttrs
      
                let 
	          ec2dynAttrs = showAttrs <$> curSelect midM EC2
	          s3dynAttrs  = showAttrs <$> curSelect midM S3
	          r53dynAttrs = showAttrs <$> curSelect midM R53
	          cfdynAttrs  = showAttrs <$> curSelect midM CF
	          rdsdynAttrs = showAttrs <$> curSelect midM RDS
	          dbdynAttrs  = showAttrs <$> curSelect midM DB
	          ecdynAttrs  = showAttrs <$> curSelect midM EC
	          cwdynAttrs  = showAttrs <$> curSelect midM CW
                  vpcdynAttrs = showAttrs <$> curSelect midM VPC
	          lsdynAttrs  = showAttrs <$> curSelect midM LS
               return [ (1  , ec2Prop)
	              , (2  , s3Prop )
	              , (3  , r53Prop)
		      , (4  , cfProp )
		      , (5  , rdsProp)
		      , (6  , dbProp )
		      , (7  , ecProp )
		      , (8  , cwProp )
		      , (9  , vpcProp)
		      , (10 , lsProp )
		      ]
	       
-- Menu summary berupa hasil perhitungan final dari semua jasa AWS
    elClass "div" "summaryMenu" $ do
      let
        resultList = (fmap . fmap) readDouble (Map.elems $ Map.fromList rightM)
	starter = readDouble <$> (constDyn "0")
	joined = join $ foldM foldTheCost starter resultList
      el "h2" $ text "Summary: "
      el "h4" $ text "Total Monthly Bill: "
      -- This is how foldM will work:
      --
      -- foldM f Dynamic t 0 [Dynamic t 5, Dynamic t 6]
      -- do
      --   a2 <- f (Dynamic t 0) (Dynamic t 5)
      --   a3 <- f (Dynamic t 5) (Dynamic t 6)
      --
      el "p" $ dynText $ (constDyn "$ ") <> (toText <$> (join $ foldM foldTheCost starter resultList))
      el "h4" $ text "Total Hourly/Month Bill: "
      el "p" $ dynText $ (constDyn "$ ") <> (toText <$> ((/720) <$> joined))

  return ()
  
-----
-- AWS Related Function
-----
foldTheCost :: (Num a, Reflex t) => Dynamic t a -> Dynamic t a -> Dynamic t (Dynamic t a)
foldTheCost x y = constDyn $ (+) <$> x <*> y


theDynCost :: (Ord k, Num k, Reflex t) => [(k, Dynamic t Text)] -> k -> Dynamic t Text
theDynCost rightM key = Maybe.fromMaybe (constDyn $ T.pack "0") $ lookup key rightM

curSelect :: Reflex t => Dynamic t (Maybe AwsIcon) -> AwsIcon -> Dynamic t Bool
curSelect dA ic = demuxed (demux dA) (Just ic)

styleMap :: T.Text -> Map.Map T.Text T.Text
styleMap c = "style" =: ("background-color: " <> c)

imgAttrs :: AwsIcon -> Map.Map T.Text T.Text
imgAttrs awi = ("src" =: (iconName awi)) <>
               ("width" =: "81") <>
	       ("height" =: "80")

idAttrs :: AwsIcon -> Map.Map T.Text T.Text
idAttrs awi = ("id" =: T.pack (show awi))

rightClassAttrs :: Reflex t => Dynamic t (Map.Map T.Text T.Text)
rightClassAttrs = constDyn ("class" =: "rightProp")


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

showAttrs :: Bool -> Map.Map T.Text T.Text
showAttrs b = "style" =: ("display: " <> display b)
  where
    display True = "inline-flex"
    display _    = "none"

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


turnToInt :: Num a => TR.Reader a -> Text -> a
turnToInt = (theValue .)
  where
    theValue (Right (v,_)) = v
    theValue (Left _)      = 0

readDouble :: Text -> Double
readDouble = turnToInt TR.double

showPrecise :: RealFloat a => a -> String
showPrecise x = showFFloat Nothing x ""

toDynText :: (Reflex t, Show a, RealFloat a) => Dynamic t a -> Dynamic t Text
toDynText = ((T.pack . showPrecise) <$>)

toText :: (Show a, RealFloat a) => a -> Text
toText = (T.pack . showPrecise)

ddUsage :: Map.Map Int T.Text
ddUsage = Map.fromList [(1,"Hours/Day"), (2, "Hours/Week"), (3, "Hours/Month")]

ddType :: Map.Map Int T.Text
ddType = Map.fromList [(1,"t2.micro"), (2, "t2.small"), (3, "t2.medium"), (4, "t2.large")]

ddStorage :: Map.Map Int T.Text
ddStorage = Map.fromList [(1,"GB"), (2, "TB"), (3, "PB")]

ddPerMonth:: Map.Map Int T.Text
ddPerMonth = Map.fromList [(1,"Per Day"), (2, "Per Week"), (3, "Per Month")]

--- properties attribut dari setiap service



-------
-- EC2 Properties
-------

calcEC2Instance :: Int -> Text -> Text
calcEC2Instance key y =
  case key of
    1 -> toText $ (*0.012) (readDouble y)
    2 -> toText $ (*0.023) (readDouble y)
    3 -> toText $ (*0.047) (readDouble y)
    4 -> toText $ (*0.094) (readDouble y)


calcEC2Usage :: Int -> Text -> Text
calcEC2Usage key y =
  case key of
    1 -> toText $ (*30) (readDouble y)
    2 -> toText $ (*4)  (readDouble y)
    3 -> toText $ (*1)  (readDouble y)

-------------------------------------------

ec2Properties :: (Reflex t, MonadWidget t m) => Dynamic t (Map.Map T.Text T.Text) -> m (Dynamic t Text)
ec2Properties dynAttrs = do
  result <- elDynAttr "div" ((constDyn $ idAttrs EC2) <> dynAttrs <> rightClassAttrs) $ do
             rec
              ec2Inst <- ec2Instance evReset
	      evReset <- button "Reset"
             return $ ec2Inst
  return $ result
  
ec2Instance :: (Reflex t, MonadWidget t m) => Event t a -> m (Dynamic t Text)
ec2Instance evReset = do
 rec
  let
      resultEc2Inst  = calcEC2Instance <$> (value ddEc2Type) <*> (value ec2Instances) 
      resultEc2Usage = calcEC2Usage <$> (value ddEc2Usage) <*> (value ec2Usage)
      resultEc2      = (*) <$> (readDouble <$> resultEc2Inst)
                           <*> (readDouble <$> resultEc2Usage)
			
  el "h4" $ text "EC2 Instances: "
  el "p" $ text "Instances:"
  ec2Instances <- textInput $ def & textInputConfig_inputType .~ "number"
                                  & textInputConfig_initialValue .~ "0"
				  & setValue .~ (leftmost ["0" <$ evReset])
  el "p" $ text "Usage:"
  ec2Usage <- textInput $ def & textInputConfig_inputType .~ "number"
                              & textInputConfig_initialValue .~ "0"
		              & setValue .~ (leftmost ["0" <$ evReset])
  ddEc2Usage <- dropdown 1 (constDyn ddUsage) def
			      
  el "p" $ text "Type:"
  ddEc2Type <- dropdown 1 (constDyn ddType) def
  
 return $ toDynText resultEc2

-------
-- S3 Properties
-------

calcS3StdStore :: Text -> Int -> Text -> Text -> Text
calcS3StdStore store key pcpl go =
  toText $
    calcStore (storeKey (readDouble store) key)
              (readDouble pcpl)
	      (readDouble go)
  where
    calcStore result pcpl go = result + (0.000005 * pcpl) + (0.0000004 * go)
    storeKey str k1 = case k1 of
      1 -> results' str
      2 -> results' (str * 1000)
      3 -> results' (str * 1000000)
     where
        results' gb
	  | gb <= 50000.0                   = 0.0256 * gb
	  | gb > 50000.0 && gb <= 500000.0  = (0.0256 * 50000.0) + (0.024 * (gb - 50000.0))
	  | otherwise                       =   (0.0256 * 50000.0)
	                                      + (0.024 * 450000.0)
					      + (0.023 * (gb - 500000.0))
	  

calcS3StoreMan :: Text -> Int -> Text -> Int -> Text -> Int -> Text
calcS3StoreMan invt ikey anly akey objc okey =
  toText $  (inventory invt ikey) + (analytics anly akey) + (objectTag objc okey)
  where
    inventory i key = case key of
      1 -> (*0.09 ) (readDouble i)
      2 -> (*0.02) (readDouble i)
      3 -> (*0.01) (readDouble i)
    analytics i key = case key of
      1 -> (*3.05) (readDouble i)
      2 -> (*0.43) (readDouble i)
      3 -> (*0.10) (readDouble i)
    objectTag i key = case key of
      1 -> (*0.31 ) (readDouble i)
      2 -> (*0.05) (readDouble i)
      3 -> (*0.01) (readDouble i)
  
--------------------

s3Properties :: (Reflex t, MonadWidget t m) => Dynamic t (Map.Map T.Text T.Text) ->  m (Dynamic t Text)
s3Properties dynAttrs = do
  result <- elDynAttr "div" ((constDyn $ idAttrs S3) <> dynAttrs <> rightClassAttrs) $ do
             rec
              s3StdS  <- s3StdStorage evReset
	      evReset <- button "Reset"
             return $ s3StdS
  return $ result

s3StdStorage :: (Reflex t, MonadWidget t m) => Event t a -> m (Dynamic t Text)
s3StdStorage evReset = do
 rec
  let
      resultS3StdStore  = calcS3StdStore  <$> (value s3StdStore)
                                          <*> (value ddS3StdStore)
					  <*> (value s3PcplReq)
					  <*> (value s3GoReq)
      resultS3StorMan   = calcS3StoreMan  <$> (value s3StoreManInv)
                                          <*> (value ddS3StoreManInv)
					  <*> (value s3StoreManAnl)
					  <*> (value ddS3StoreManAnl)
					  <*> (value s3StoreManObj)
					  <*> (value ddS3StoreManObj)
      resultS3Store     = (+) <$> (readDouble <$> resultS3StdStore)
                              <*> (readDouble <$> resultS3StorMan)
			      
  -- Standard Storage Properties			
  el "h4" $ text "Standard Storage: "
  el "p" $ text "Storage:"
  s3StdStore <- textInput $ def & textInputConfig_inputType .~ "number"
                                  & textInputConfig_initialValue .~ "0"
				  & setValue .~ (leftmost ["0" <$ evReset])
  ddS3StdStore <- dropdown 1 (constDyn ddStorage) def
  
  el "p" $ text "PUT/COPY/POST/LIST Request:"
  s3PcplReq <- textInput $ def & textInputConfig_inputType .~ "number"
                               & textInputConfig_initialValue .~ "0"
		               & setValue .~ (leftmost ["0" <$ evReset])
  el "p" $ text "Request"
  
  el "p" $ text "GET and Other Request:"
  s3GoReq <- textInput $ def & textInputConfig_inputType .~ "number"
                             & textInputConfig_initialValue .~ "0"
		             & setValue .~ (leftmost ["0" <$ evReset])
  el "p" $ text "Request"
  
  -- Storage Management Properties
  el "h4" $ text "Storage Management: "
  el "p" $ text "Inventory"
  el "p" $ text "(Million Objects):"

  s3StoreManInv <- textInput $ def & textInputConfig_inputType .~ "number"
                                  & textInputConfig_initialValue .~ "0"
				  & setValue .~ (leftmost ["0" <$ evReset])
  ddS3StoreManInv <- dropdown 3 (constDyn ddPerMonth) def

  el "p" $ text "Analytics"
  el "p" $ text "(Million Objects):"

  s3StoreManAnl <- textInput $ def & textInputConfig_inputType .~ "number"
                                  & textInputConfig_initialValue .~ "0"
				  & setValue .~ (leftmost ["0" <$ evReset])
  ddS3StoreManAnl <- dropdown 3 (constDyn ddPerMonth) def

  el "p" $ text "Object Tagging"
  el "p" $ text "(10,000 Tags):"

  s3StoreManObj <- textInput $ def & textInputConfig_inputType .~ "number"
                                  & textInputConfig_initialValue .~ "0"
				  & setValue .~ (leftmost ["0" <$ evReset])
  ddS3StoreManObj <- dropdown 3 (constDyn ddPerMonth) def
  
 return $ toDynText resultS3Store

-------
-- Route53 Properties
-------

r53Properties :: (Reflex t, MonadWidget t m) => Dynamic t (Map.Map T.Text T.Text) -> m (Dynamic t Text)
r53Properties dynAttrs = do
  result <- elDynAttr "div" ((constDyn $ idAttrs R53) <> dynAttrs <> rightClassAttrs) $ do
             rec
              r53HostZ <- r53HostedZone evReset
	      evReset <- button "Reset"
             return $ r53HostZ
  return $ result
  
r53HostedZone :: (Reflex t, MonadWidget t m) => Event t a -> m (Dynamic t Text)
r53HostedZone evReset = do
 rec
  let
      resultR53Hz = _textInput_value r53Hz
      resultR53Tf = _textInput_value r53Tf
      resultR53   = (+) <$> (readDouble <$> resultR53Hz)
                        <*> (readDouble <$> resultR53Tf)
  
  el "h4" $ text "Hosted Zone: "
  el "p" $ text "Hosted Zone:"
  r53Hz <- textInput $ def & textInputConfig_inputType .~ "number"
                           & textInputConfig_initialValue .~ "0"
			   & setValue .~ (leftmost ["0" <$ evReset])
			   
  el "p" $ text "Traffic Flow:"
  r53Tf <- textInput $ def & textInputConfig_inputType .~ "number"
                           & textInputConfig_initialValue .~ "0"
			   & setValue .~ (leftmost ["0" <$ evReset])			   
 return $ toDynText resultR53
  
-------
-- CloudFront Properties
-------

cfProperties :: (Reflex t, MonadWidget t m) => Dynamic t (Map.Map T.Text T.Text) -> m (Dynamic t Text)
cfProperties dynAttrs =
  elDynAttr "div" ((constDyn $ idAttrs CF) <> dynAttrs <> rightClassAttrs) $ do
   rec
    el "p" $ text "CloudFront"
   return $ constDyn (T.pack "0")
   
-------
-- Amazon RDS Properties
-------

rdsProperties :: (Reflex t, MonadWidget t m) => Dynamic t (Map.Map T.Text T.Text) -> m (Dynamic t Text)
rdsProperties dynAttrs =
  elDynAttr "div" ((constDyn $ idAttrs RDS) <> dynAttrs <> rightClassAttrs) $ do
   rec
    el "p" $ text "Amazon RDS"
   return $ constDyn (T.pack "0")
   
-------
-- DynamoDB Properties
-------

dbProperties :: (Reflex t, MonadWidget t m) => Dynamic t (Map.Map T.Text T.Text) -> m (Dynamic t Text)
dbProperties dynAttrs =
  elDynAttr "div" ((constDyn $ idAttrs DB) <> dynAttrs <> rightClassAttrs) $ do
   rec
    el "p" $ text "DynamoDB"
   return $ constDyn (T.pack "0")
   
-------
-- Elastic Cache Properties
-------

ecProperties :: (Reflex t, MonadWidget t m) => Dynamic t (Map.Map T.Text T.Text) -> m (Dynamic t Text)
ecProperties dynAttrs =
  elDynAttr "div" ((constDyn $ idAttrs EC) <> dynAttrs <> rightClassAttrs) $ do
   rec
    el "p" $ text "ElasticCache"
   return $ constDyn (T.pack "0")
   
-------
-- CloudWatch Properties
-------

cwProperties :: (Reflex t, MonadWidget t m) => Dynamic t (Map.Map T.Text T.Text) -> m (Dynamic t Text)
cwProperties dynAttrs =
  elDynAttr "div" ((constDyn $ idAttrs CW) <> dynAttrs <> rightClassAttrs) $ do
   rec
    el "p" $ text "CloudWatch"
   return $ constDyn (T.pack "0")
   
-------
-- Amazon VPC Properties
-------

vpcProperties :: (Reflex t, MonadWidget t m) => Dynamic t (Map.Map T.Text T.Text) -> m (Dynamic t Text)
vpcProperties dynAttrs =
  elDynAttr "div" ((constDyn $ idAttrs VPC) <> dynAttrs <> rightClassAttrs) $ do
   rec
    el "p" $ text "Amazon VPC"
   return $ constDyn (T.pack "0")
   
-------
-- Amazon LightSail Properties
-------

lsProperties :: (Reflex t, MonadWidget t m) => Dynamic t (Map.Map T.Text T.Text) -> m (Dynamic t Text)
lsProperties dynAttrs =
  elDynAttr "div" ((constDyn $ idAttrs LS) <> dynAttrs <> rightClassAttrs) $ do
   rec
    el "p" $ text "Amazon LightSail"
   return $ constDyn (T.pack "0")

--------

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
  el "h4" $ text "Maks length 14"
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