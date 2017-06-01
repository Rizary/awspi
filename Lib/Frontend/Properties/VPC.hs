{-# LANGUAGE OverloadedStrings, RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds #-}

module Frontend.Properties.VPC
  (
    vpcProperties
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
-- Amazon VPC Properties
-------

calcVPCConn :: T.Text -> Int -> T.Text -> T.Text
calcVPCConn num key y =
  case key of
    1 -> toText $ (price * (readDouble num)) * (30 * (readDouble y))
    2 -> toText $ (price * (readDouble num)) * (4 * (readDouble y))
    _ -> toText $ (price * (readDouble num)) * (readDouble y)
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
