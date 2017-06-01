{-# LANGUAGE OverloadedStrings, RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds #-}

module AWSFunction where

import Prelude hiding (mapM, mapM_, all, sequence)

import           Control.Monad hiding (mapM, mapM_, forM, forM_, sequence)
import qualified Data.Map                  as Map
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text                 as T
import qualified Data.Text.Read            as TR
import qualified Data.Maybe                as Maybe
import           Numeric (showFFloat)
import           Reflex
import           Reflex.Dom.Core

-----
-- AWS Related Function
-----

data AwsIcon = CF | DB | EC2 | EC | LS | RDS | R53 | S3 | VPC
  deriving (Eq, Show, Ord)

iconName :: AwsIcon -> T.Text
iconName awi =
  case awi of
    CF  -> "static/svg/AmazonCloudFront.png"
    DB  -> "static/svg/AmazonDynamoDB.png"
    EC2 -> "static/svg/AmazonEC2.png"
    EC  -> "static/svg/AmazonElasticCache.png"
    LS  -> "static/svg/AmazonLightsail.png"
    RDS -> "static/svg/AmazonRDS.png"
    R53 -> "static/svg/AmazonRoute53.png"
    S3  -> "static/svg/AmazonS3.png"
    VPC -> "static/svg/AmazonVPC.png"
  
foldTheCost :: (Num a, Reflex t) => Dynamic t a -> Dynamic t a -> Dynamic t (Dynamic t a)
foldTheCost x y = constDyn $ (+) <$> x <*> y


theDynCost :: Reflex t => [(Int, Dynamic t Text)] -> Int -> Dynamic t Text
theDynCost rightM key = Maybe.fromMaybe (constDyn $ T.pack "0") $ lookup key rightM

curSelect :: Reflex t => Dynamic t (Maybe AwsIcon) -> AwsIcon -> Dynamic t Bool
curSelect dA ic = demuxed (demux dA) (Just ic)

imgAttrs :: AwsIcon -> Map.Map T.Text T.Text
imgAttrs awi = ("src" =: (iconName awi)) <>
               ("width" =: "81") <>
	       ("height" =: "80")

idAttrs :: AwsIcon -> Map.Map T.Text T.Text
idAttrs awi = ("id" =: T.pack (show awi))

rightClassAttrs :: Reflex t => Dynamic t (Map.Map T.Text T.Text)
rightClassAttrs = constDyn ("class" =: "rightProp")

showAttrs :: Bool -> Map.Map T.Text T.Text
showAttrs b = "style" =: ("display: " <> display' b)
  where
    display' True = "inline-flex"
    display' _    = "none"

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
  _ -> do
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