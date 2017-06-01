{-# LANGUAGE OverloadedStrings, RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds #-}

module Frontend.Properties.RDS
  (
    rdsProperties
  ) where

import Prelude hiding (mapM, mapM_, all, sequence)
import qualified Data.Map                  as Map
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text                 as T
import Reflex
import Reflex.Dom.Core
------

import AWSFunction
--------------------------------------------------------------------------
-- Amazon RDS Properties
-------

calcRDSInst :: T.Text -> T.Text -> Int -> Int -> Int -> Int -> T.Text -> T.Text
calcRDSInst rdsi rdsu ukey ekey ckey dkey rdss =
  case ekey of
    1 -> toText $ (readDouble rdsu) * (calcUsage' ukey) * (calcEngine' dkey ckey) * (readDouble rdss)
    _ -> toText $ (readDouble rdsu) * (calcUsage' ukey) * (calcEngine'' dkey ckey) * (readDouble rdss)
  where
    calcUsage' x =
      case x of
        1 -> 30
        2 -> 4
        _ -> 1
    calcEngine' dk ck =
      case dk of
        1 -> calcDeploy' ck (readDouble rdsi)
        _ -> calcDeploy'' ck (readDouble rdsi)
     where
      calcDeploy' ck' rds' =
        case ck' of
	  1 -> rds' * 0.026
	  2 -> rds' * 0.052
	  3 -> rds' * 0.104
	  _ -> rds' * 0.209
      calcDeploy'' ck' rds' =
        case ck' of
	  1 -> rds' * 0.052
	  2 -> rds' * 0.104
	  3 -> rds' * 0.208
	  _ -> rds' * 0.418
    calcEngine'' dk ck =
      case dk of
        1 -> calcDeploy' ck (readDouble rdsi)
        _ -> calcDeploy'' ck (readDouble rdsi)
     where
      calcDeploy' ck' rds' =
        case ck' of
	  1 -> rds' * 0.028
	  2 -> rds' * 0.056
	  3 -> rds' * 0.112
	  _ -> rds' * 0.224
      calcDeploy'' ck' rds' =
        case ck' of
	  1 -> rds' * 0.056
	  2 -> rds' * 0.112
	  3 -> rds' * 0.224
	  _ -> rds' * 0.448
    
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

   