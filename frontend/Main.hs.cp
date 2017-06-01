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
              -- cw  <- serviceButton "CloudWatch" CW cwdynAttrs (theDynCost rightM 8)
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
	          -- cwdynAttrs  = labelAttrs <$> curSelect dynSelect CW
	          vpcdynAttrs = labelAttrs <$> curSelect dynSelect VPC
	          lsdynAttrs  = labelAttrs <$> curSelect dynSelect LS
              dynSelect <- holdDyn Nothing $ Just <$> leftmost [ec2, s3, r53, cf, rds, db, ec, vpc, ls]
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
                -- cwProp  <- cwProperties cwdynAttrs
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
	          -- cwdynAttrs  = showAttrs <$> curSelect midM CW
                  vpcdynAttrs = showAttrs <$> curSelect midM VPC
	          lsdynAttrs  = showAttrs <$> curSelect midM LS
               return [ (1  , ec2Prop)
	              , (2  , s3Prop )
	              , (3  , r53Prop)
		      , (4  , cfProp )
		      , (5  , rdsProp)
		      , (6  , dbProp )
		      , (7  , ecProp )
		     -- , (8  , cwProp )
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


theDynCost :: Reflex t => [(Int, Dynamic t Text)] -> Int -> Dynamic t Text
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
    -- CW  -> "static/svg/AmazonCloudWatch.png"
    DB  -> "static/svg/AmazonDynamoDB.png"
    EC2 -> "static/svg/AmazonEC2.png"
    EC  -> "static/svg/AmazonElasticCache.png"
    LS  -> "static/svg/AmazonLightsail.png"
    RDS -> "static/svg/AmazonRDS.png"
    R53 -> "static/svg/AmazonRoute53.png"
    S3  -> "static/svg/AmazonS3.png"
    VPC  -> "static/svg/AmazonVPC.png"
    _ -> ""

data AwsIcon = CF | DB | EC2 | EC | LS | RDS | R53 | S3 | VPC
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


lsRadioBtn :: (MonadWidget t m) =>  T.Text
			        -> Int
				-> m (Event t Int)
lsRadioBtn group rid = do
    ev <- elAttr "label" ("class" =: "lsRad") $ do
        (rb1, _) <- elAttr' "input" ("name" =: group <> "type" =: "radio" <> "value" =: T.pack (show rid))  blank
        lsClicked rid
        return $ domEvent Click rb1
    return $ rid <$ ev

lsClicked :: (MonadWidget t m) => Int -> m ()
lsClicked rid = case rid of
  1 -> do
        text "$ 5/Month"
	el "p" $ text "512 MB Memory"
	el "p" $ text "1 Core Processor"
	el "p" $ text "20 GB SSD Disk"
	el "p" $ text "1 TB Transfer"
        return ()
  2 -> do
        text "$ 10/Month"
	el "p" $ text "1 GB Memory"
	el "p" $ text "1 Core Processor"
	el "p" $ text "30 GB SSD Disk"
	el "p" $ text "2 TB Transfer"
        return ()
  3 -> do
        text "$ 20/Month"
	el "p" $ text "2 GB Memory"
	el "p" $ text "1 Core Processor"
	el "p" $ text "40 GB SSD Disk"
	el "p" $ text "3 TB Transfer"
        return ()
  4 -> do
        text "$ 40/Month"
	el "p" $ text " 4 GB Memory"
	el "p" $ text "2 Core Processor"
	el "p" $ text "60 GB SSD Disk"
	el "p" $ text "4 TB Transfer"
        return ()
  5 -> do
        text "$ 80/Month"
	el "p" $ text "8 GB Memory"
	el "p" $ text "2 Core Processor"
	el "p" $ text "80 GB SSD Disk"
	el "p" $ text "5 TB Transfer"
        return ()

turnToInt :: Num a => TR.Reader a -> T.Text -> a
turnToInt = (theValue .)
  where
    theValue (Right (v,_)) = v
    theValue (Left _)      = 0

readDouble :: T.Text -> Double
readDouble = turnToInt TR.double

showPrecise :: RealFloat a => a -> String
showPrecise x = showFFloat Nothing x ""

toDynText :: (Reflex t, Show a, RealFloat a) => Dynamic t a -> Dynamic t T.Text
toDynText = ((T.pack . showPrecise) <$>)

toText :: (Show a, RealFloat a) => a -> Text
toText = (T.pack . showPrecise)

ddUsage :: Map.Map Int T.Text
ddUsage = Map.fromList [(1,"Hours/Day"), (2, "Hours/Week"), (3, "Hours/Month")]

ddType :: Map.Map Int T.Text
ddType = Map.fromList [(1,"t2.micro"), (2, "t2.small"), (3, "t2.medium"), (4, "t2.large")]

ddNode :: Map.Map Int T.Text
ddNode = Map.fromList [(1,"t2.micro"), (2, "t2.small"), (3, "t2.medium")]

ddStorage :: Map.Map Int T.Text
ddStorage = Map.fromList [(1,"GB"), (2, "TB"), (3, "PB")]

ddPerMonth :: Map.Map Int T.Text
ddPerMonth = Map.fromList [(1,"Per Day"), (2, "Per Week"), (3, "Per Month")]

ddVolume :: Map.Map Int T.Text
ddVolume = Map.fromList [(1,"GB/Day"), (2, "GB/Week"), (3, "GB/Month"), (4, "TB/Month")]

ddDataSet :: Map.Map Int T.Text
ddDataSet = Map.fromList [(1,"GB"), (2, "TB")]

ddEngine :: Map.Map Int T.Text
ddEngine = Map.fromList [(1,"MariaDB"), (2, "PostgreSQL")]

ddDeploy :: Map.Map Int T.Text
ddDeploy = Map.fromList [(1,"Standard (Single-AZ"), (2, "Multi-AZ")]

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

calcR53ZoneF :: Text -> Text -> Text
calcR53ZoneF hz tf = toText $ hosZ (readDouble hz) + (50.0 * (readDouble tf))
  where
    hosZ hz'
      | hz' <= 25  = 0.50 * hz'
      | otherwise  = (0.50 * 25) + (0.10 * (hz' -  25))

calcR53Queries :: Text -> Int -> Text -> Int -> Text -> Int -> Text
calcR53Queries sq skey lbrq lkey gdq gkey =
  toText $   (standardQ (readDouble sq) skey)
           + (latencyQ (readDouble lbrq) lkey)
	   + (geoDnsQ (readDouble gdq) gkey)
  where
    standardQ i k1 = case k1 of
      1 -> 30 * standardQ' i
      2 -> 4 * standardQ' i
      3 -> standardQ' i
      where
       standardQ' sq
         | sq <= 1000 = 0.400 * sq
	 | otherwise  = (0.400 * 1000) + (0.200 * (sq - 1000))
	 
    latencyQ i k2 = case k2 of
      1 -> 30 * latencyQ' i
      2 -> 4 * latencyQ' i
      3 -> latencyQ' i
      where
       latencyQ' lq
         | lq <= 1000 = 0.600 * lq
	 | otherwise  = (0.600 * 1000) + (0.300 * (lq - 1000))
	 
    geoDnsQ i k3 = case k3 of
      1 -> 30 * geoDnsQ' i
      2 -> 4 * geoDnsQ' i
      3 -> geoDnsQ' i
      where
       geoDnsQ' gq
         | gq <= 1000 = 0.700 * gq
	 | otherwise  = (0.700 * 1000) + (0.350 * (gq - 1000))

-------------------------------------

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
      resultR53ZoneF   = calcR53ZoneF    <$> (value r53Hz)
                                         <*> (value r53Tf)

      resultR53Queries = calcR53Queries  <$> (value r53Sq)
                                         <*> (value ddR53Sq)
					 <*> (value r53Lbrq)
					 <*> (value ddR53Lbrq)
					 <*> (value r53GeoDQ)
					 <*> (value ddR53GeoDQ)
      resultR53HZone    = (+) <$> (readDouble <$> resultR53ZoneF)
                              <*> (readDouble <$> resultR53Queries)
  
  el "h4" $ text "Hosted Zone: "
  el "p" $ text "Hosted Zone:"
  r53Hz <- textInput $ def & textInputConfig_inputType .~ "number"
                           & textInputConfig_initialValue .~ "0"
			   & setValue .~ (leftmost ["0" <$ evReset])
			   
  el "p" $ text "Traffic Flow:"
  r53Tf <- textInput $ def & textInputConfig_inputType .~ "number"
                           & textInputConfig_initialValue .~ "0"
			   & setValue .~ (leftmost ["0" <$ evReset])

  el "p" $ text "Standard Queries"
  el "p" $ text "(in Million Queries):"

  r53Sq <- textInput $ def & textInputConfig_inputType .~ "number"
                                  & textInputConfig_initialValue .~ "0"
				  & setValue .~ (leftmost ["0" <$ evReset])
  ddR53Sq <- dropdown 3 (constDyn ddPerMonth) def

  el "p" $ text "Latency Based"
  el "p" $ text "Routing Queries"
  el "p" $ text "(in Million Queries):"

  r53Lbrq <- textInput $ def & textInputConfig_inputType .~ "number"
                                  & textInputConfig_initialValue .~ "0"
				  & setValue .~ (leftmost ["0" <$ evReset])
  ddR53Lbrq <- dropdown 3 (constDyn ddPerMonth) def

  el "p" $ text "Geo DNS Queries"
  el "p" $ text "(in Million Queries):"

  r53GeoDQ <- textInput $ def & textInputConfig_inputType .~ "number"
                                  & textInputConfig_initialValue .~ "0"
				  & setValue .~ (leftmost ["0" <$ evReset])
  ddR53GeoDQ <- dropdown 3 (constDyn ddPerMonth) def

 return $ toDynText resultR53HZone
  
-------
-- CloudFront Properties
-------

calcCFVolume :: Text -> Int -> Text
calcCFVolume cmv ckey = toText $
  case ckey of
    1 -> (*30) (price * (readDouble cmv)) 
    2 -> (*4) (price * (readDouble cmv)) 
    3 -> (price * (readDouble cmv)) 
    4 -> (*1000) (price * (readDouble cmv))
  where
    price = 0.025

--------------------------

cfProperties :: (Reflex t, MonadWidget t m) => Dynamic t (Map.Map T.Text T.Text) -> m (Dynamic t Text)
cfProperties dynAttrs = do
 rec
  result <- elDynAttr "div" ((constDyn $ idAttrs CF) <> dynAttrs <> rightClassAttrs) $ do
             rec
              cfDTO <- cfDataTransfer evReset
	      evReset <- button "Reset"
             return $ cfDTO
 return $ result

cfDataTransfer :: (Reflex t, MonadWidget t m) => Event t a -> m (Dynamic t Text)
cfDataTransfer evReset = do
 rec
  let
      resultCFVolume  = calcCFVolume <$> (value cfMonthlyV)
                                     <*> (value ddCFMonthlyV)
			      
  el "h4" $ text "Data Transfer Out: "
  el "p" $ text "Monthly Volume:"
  cfMonthlyV <- textInput $ def & textInputConfig_inputType .~ "number"
                                  & textInputConfig_initialValue .~ "0"
				  & setValue .~ (leftmost ["0" <$ evReset])
  ddCFMonthlyV <- dropdown 1 (constDyn ddVolume) def
  
 return $ resultCFVolume
-------
-- Amazon RDS Properties
-------

calcRDSInst :: T.Text -> T.Text -> Int -> Int -> Int -> Int -> T.Text -> T.Text
calcRDSInst rdsi rdsu ukey ekey ckey dkey rdss =
  case ekey of
    1 -> toText $ (readDouble rdsu) * (calcUsage' ukey) * (calcEngine' dkey ckey) * (readDouble rdss)
    2 -> toText $ (readDouble rdsu) * (calcUsage' ukey) * (calcEngine'' dkey ckey) * (readDouble rdss)
  where
    calcUsage' x =
      case x of
        1 -> 30
        2 -> 4
        3 -> 1
    calcEngine' dk ck =
      case dk of
        1 -> calcDeploy' ck (readDouble rdsi)
        2 -> calcDeploy'' ck (readDouble rdsi)
     where
      calcDeploy' ck' rds' =
        case ck' of
	  1 -> rds' * 0.026
	  2 -> rds' * 0.052
	  3 -> rds' * 0.104
	  4 -> rds' * 0.209
      calcDeploy'' ck' rds' =
        case ck' of
	  1 -> rds' * 0.052
	  2 -> rds' * 0.104
	  3 -> rds' * 0.208
	  4 -> rds' * 0.418
    calcEngine'' dk ck =
      case dk of
        1 -> calcDeploy' ck (readDouble rdsi)
        2 -> calcDeploy'' ck (readDouble rdsi)
     where
      calcDeploy' ck' rds' =
        case ck' of
	  1 -> rds' * 0.028
	  2 -> rds' * 0.056
	  3 -> rds' * 0.112
	  4 -> rds' * 0.224
      calcDeploy'' ck' rds' =
        case ck' of
	  1 -> rds' * 0.056
	  2 -> rds' * 0.112
	  3 -> rds' * 0.224
	  4 -> rds' * 0.448
    
------------------

rdsProperties :: (Reflex t, MonadWidget t m) => Dynamic t (Map.Map T.Text T.Text) -> m (Dynamic t Text)
rdsProperties dynAttrs = do
  result <- elDynAttr "div" ((constDyn $ idAttrs RDS) <> dynAttrs <> rightClassAttrs) $ do
             rec
              rdsInst <- rdsInstance evReset
	      evReset <- button "Reset"
             return $ rdsInst
  return $ result

rdsInstance :: (Reflex t, MonadWidget t m) => Event t a -> m (Dynamic t Text)
rdsInstance evReset = do
 rec
  let
      resultRds = calcRDSInst <$> (value rdsInstances)
                              <*> (value rdsUsage)
			      <*> (value ddRdsUsage)
			      <*> (value ddRdsEngine)
			      <*> (value ddRdsClass)
			      <*> (value ddRdsDeploy)
			      <*> (value rdsStore)
			
  el "h4" $ text "RDS On-Demand DB Instances: "
  el "p" $ text "Instances:"
  rdsInstances <- textInput $ def & textInputConfig_inputType .~ "number"
                               & textInputConfig_initialValue .~ "0"
			       & setValue .~ (leftmost ["0" <$ evReset])
  el "p" $ text "Usage:"
  rdsUsage <- textInput $ def & textInputConfig_inputType .~ "number"
                              & textInputConfig_initialValue .~ "0"
		              & setValue .~ (leftmost ["0" <$ evReset])
  ddRdsUsage <- dropdown 1 (constDyn ddUsage) def

  el "p" $ text "UsageDB Engine"
  el "p" $ text "and License: "
  ddRdsEngine <- dropdown 1 (constDyn ddEngine) def

  el "p" $ text "Class and Deployment: "
  el "p" $ blank
  ddRdsClass <- dropdown 1 (constDyn ddType) def
  ddRdsDeploy <- dropdown 1 (constDyn ddDeploy) def

  el "p" $ text "Storage (in GB): "
  rdsStore <- textInput $ def & textInputConfig_inputType .~ "number"
                              & textInputConfig_initialValue .~ "0"
		              & setValue .~ (leftmost ["0" <$ evReset])
 return $ resultRds

   
-------
-- DynamoDB Properties
-------

calcDBData :: Text -> Int -> Text
calcDBData cmv ckey = toText $
  case ckey of
    1 -> calcDB' $ readDouble cmv
    2 -> calcDB' $ 1000 * (readDouble cmv)

  where
    calcDB' cmv'
      | cmv' <= 25 = 0
      | otherwise  = price * (cmv' - 25)
    price = 0.285
    
--------------------
dbProperties :: (Reflex t, MonadWidget t m) => Dynamic t (Map.Map T.Text T.Text) -> m (Dynamic t Text)
dbProperties dynAttrs = do
 rec
  result <- elDynAttr "div" ((constDyn $ idAttrs CF) <> dynAttrs <> rightClassAttrs) $ do
             rec
              dbDts <- dbDataSet evReset
	      evReset <- button "Reset"
             return $ dbDts
 return $ result

dbDataSet :: (Reflex t, MonadWidget t m) => Event t a -> m (Dynamic t Text)
dbDataSet evReset = do
 rec
  let
      resultDBData   = calcDBData <$> (value dbDataSize)
                                  <*> (value ddDBDataSize)
			      
  el "h4" $ text "Data Transfer Out: "
  el "p" $ text "Monthly Volume:"
  dbDataSize <- textInput $ def & textInputConfig_inputType .~ "number"
                                  & textInputConfig_initialValue .~ "0"
				  & setValue .~ (leftmost ["0" <$ evReset])
  ddDBDataSize <- dropdown 1 (constDyn ddDataSet) def
  
 return $ resultDBData
   
-------
-- Elastic Cache Properties
-------

calcECNode :: Int -> Text -> Text
calcECNode key y =
  case key of
    1 -> toText $ (*0.022) (readDouble y)
    2 -> toText $ (*0.052) (readDouble y)
    3 -> toText $ (*0.104) (readDouble y)

calcECUsage :: Int -> Text -> Text
calcECUsage key y =
  case key of
    1 -> toText $ (*30) (readDouble y)
    2 -> toText $ (*4)  (readDouble y)
    3 -> toText $ (*1)  (readDouble y)
---------------------------

ecProperties :: (Reflex t, MonadWidget t m) => Dynamic t (Map.Map T.Text T.Text) -> m (Dynamic t Text)
ecProperties dynAttrs = do
  result <- elDynAttr "div" ((constDyn $ idAttrs EC) <> dynAttrs <> rightClassAttrs) $ do
             rec
              ecInst <- ecInstance evReset
	      evReset <- button "Reset"
             return $ ecInst
  return $ result
  
ecInstance :: (Reflex t, MonadWidget t m) => Event t a -> m (Dynamic t Text)
ecInstance evReset = do
 rec
  let
      resultEcNode  = calcECNode  <$> (value ddEcType) <*> (value ecNodes) 
      resultEcUsage = calcECUsage <$> (value ddEcUsage) <*> (value ecUsage)
      resultEc      = (*) <$> (readDouble <$> resultEcNode)
                          <*> (readDouble <$> resultEcUsage)
			
  el "h4" $ text "On-Demand Cache Nodes: "
  el "p" $ text "Nodes:"
  ecNodes <- textInput $ def & textInputConfig_inputType .~ "number"
                             & textInputConfig_initialValue .~ "0"
			     & setValue .~ (leftmost ["0" <$ evReset])
  el "p" $ text "Usage:"
  ecUsage <- textInput $ def & textInputConfig_inputType .~ "number"
                              & textInputConfig_initialValue .~ "0"
		              & setValue .~ (leftmost ["0" <$ evReset])
  ddEcUsage <- dropdown 1 (constDyn ddUsage) def
			      
  el "p" $ text "Node Type:"
  ddEcType <- dropdown 1 (constDyn ddNode) def
  
 return $ toDynText resultEc
-------
-- CloudWatch Properties
-------
{-|
cwProperties :: (Reflex t, MonadWidget t m) => Dynamic t (Map.Map T.Text T.Text) -> m (Dynamic t Text)
cwProperties dynAttrs =
  elDynAttr "div" ((constDyn $ idAttrs CW) <> dynAttrs <> rightClassAttrs) $ do
   rec
    el "p" $ text "CloudWatch"
   return $ constDyn (T.pack "0")
-}   
-------
-- Amazon VPC Properties
-------

calcVPCConn :: T.Text -> Int -> T.Text -> T.Text
calcVPCConn num key y =
  case key of
    1 -> toText $ (price * (readDouble num)) * (30 * (readDouble y))
    2 -> toText $ (price * (readDouble num)) * (4 * (readDouble y))
    3 -> toText $ (price * (readDouble num)) * (readDouble y)
  where
    price = 0.05

----------------------------------

vpcProperties :: (Reflex t, MonadWidget t m) => Dynamic t (Map.Map T.Text T.Text) -> m (Dynamic t Text)
vpcProperties dynAttrs = do
  result <- elDynAttr "div" ((constDyn $ idAttrs VPC) <> dynAttrs <> rightClassAttrs) $ do
             rec
              vpcCon <- vpcConnections evReset
	      evReset <- button "Reset"
             return $ vpcCon
  return $ result

vpcConnections :: (Reflex t, MonadWidget t m) => Event t a -> m (Dynamic t Text)
vpcConnections evReset = do
 rec
  let
    resultVpc = calcVPCConn <$> (value vpcNumCon)
                            <*> (value ddEcUsage)
			    <*> (value vpcUsage)
			
  el "h4" $ text "VPN Connections: "
  el "p" $ text "Number of Connections:"
  vpcNumCon <- textInput $ def & textInputConfig_inputType .~ "number"
                               & textInputConfig_initialValue .~ "0"
			       & setValue .~ (leftmost ["0" <$ evReset])
  el "p" $ text "Usage:"
  vpcUsage <- textInput $ def & textInputConfig_inputType .~ "number"
                              & textInputConfig_initialValue .~ "0"
		              & setValue .~ (leftmost ["0" <$ evReset])
  ddEcUsage <- dropdown 1 (constDyn ddUsage) def
  
 return $ resultVpc

   
-------
-- Amazon LightSail Properties
-------

calcLsInstance :: Text -> Text -> Int -> Text -> Text
calcLsInstance num y key rad =
  case key of
    1 -> toText $ calcLS' (30 * readDouble y) (readDouble rad) (readDouble num)
    2 -> toText $ calcLS' (4 * readDouble y) (readDouble rad) (readDouble num)
    3 -> toText $ calcLS' (readDouble y) (readDouble rad) (readDouble num)
    4 -> toText $ calcLS' (1000 * readDouble y) (readDouble rad) (readDouble num)
    _ -> toText 0
  where
    price = 0.09
    calcLS' dt pp num' = case pp of
      1 -> if dt <= 1000 then 5  * num' else 5  * num' + ((dt - 1000) * price)
      2 -> if dt <= 2000 then 10 * num' else 10 * num' + ((dt - 2000) * price)
      3 -> if dt <= 3000 then 20 * num' else 20 * num' + ((dt - 3000) * price)
      4 -> if dt <= 4000 then 40 * num' else 40 * num' + ((dt - 4000) * price)
      5 -> if dt <= 5000 then 80 * num' else 80 * num' + ((dt - 5000) * price)
      _ -> 0
      

-------------------

lsProperties :: (Reflex t, MonadWidget t m) => Dynamic t (Map.Map T.Text T.Text) -> m (Dynamic t Text)
lsProperties dynAttrs = do
  result <- elDynAttr "div" ((constDyn $ idAttrs LS) <> dynAttrs <> rightClassAttrs) $ do
             rec
              lsInst  <- lsInstance evReset
	      evReset <- button "Reset"
             return $ lsInst
  return $ result

lsInstance :: (Reflex t, MonadWidget t m) => Event t a -> m (Dynamic t Text)
lsInstance evReset = do
 rec
  let
    resultLS = calcLsInstance <$> (value lsInstances)
                              <*> (value lsUsage)
			      <*> (value ddLSUsage)
			      <*> evRadio    
    group = "gLS"
			
  el "h4" $ text "VPN Connections: "
  el "p" $ text "Number of Connections:"
  lsInstances <- textInput $ def & textInputConfig_inputType .~ "number"
                                 & textInputConfig_initialValue .~ "0"
			         & setValue .~ (leftmost ["0" <$ evReset])
  el "p" $ text "Usage:"
  lsUsage <- textInput $ def & textInputConfig_inputType .~ "number"
                             & textInputConfig_initialValue .~ "0"
		             & setValue .~ (leftmost ["0" <$ evReset])
			     
  ddLSUsage <- dropdown 1 (constDyn ddVolume) def
  
  el "p" $ text "Pricing Plan:"
  evRad1 <- lsRadioBtn group 1 
  evRad2 <- lsRadioBtn group 2 
  evRad3 <- lsRadioBtn group 3 
  evRad4 <- lsRadioBtn group 4 
  evRad5 <- lsRadioBtn group 5

  evRadio <- holdDyn "1" $ (T.pack . show) <$> leftmost [evRad1, evRad2, evRad3, evRad4, evRad5]
  
 return $ resultLS
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