{-# LANGUAGE OverloadedStrings, RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds #-}

module Frontend.Properties.CF
  (
    cfProperties
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
-- CloudFront Properties
-------

calcCFVolume :: Text -> Int -> Text
calcCFVolume cmv ckey = toText $
  case ckey of
    1 -> (*30) (price * (readDouble cmv)) 
    2 -> (*4) (price * (readDouble cmv)) 
    3 -> (price * (readDouble cmv)) 
    _ -> (*1000) (price * (readDouble cmv))
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