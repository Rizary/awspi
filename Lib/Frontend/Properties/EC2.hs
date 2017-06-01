{-# LANGUAGE OverloadedStrings, RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds #-}

module Frontend.Properties.EC2
  (
    ec2Properties
  ) where

import Prelude hiding (mapM, mapM_, all, sequence)
import qualified Data.Map                  as Map
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text                 as T
import Reflex
import Reflex.Dom.Core

------------

import AWSFunction


--------------------------------------------------------------------------
-- EC2 Properties
-------

calcEC2Instance :: Int -> Text -> Text
calcEC2Instance key y =
  case key of
    1 -> toText $ (*0.015) (readDouble y)
    2 -> toText $ (*0.030) (readDouble y)
    3 -> toText $ (*0.060) (readDouble y)
    _ -> toText $ (*0.120) (readDouble y)


calcEC2Usage :: Int -> Text -> Text
calcEC2Usage key y =
  case key of
    1 -> toText $ (*30) (readDouble y)
    2 -> toText $ (*4)  (readDouble y)
    _ -> toText $ (*1)  (readDouble y)

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
