{-# LANGUAGE OverloadedStrings, RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds #-}

module Frontend.Properties.LS
  (
    lsProperties
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
-- Amazon LightSail Properties
-------

calcLsInstance :: Text -> Text -> Int -> Text -> Text
calcLsInstance num y key rad =
  case key of
    1 -> toText $ calcLS' (30 * readDouble y) (readDouble rad) (readDouble num)
    2 -> toText $ calcLS' (4 * readDouble y) (readDouble rad) (readDouble num)
    3 -> toText $ calcLS' (readDouble y) (readDouble rad) (readDouble num)
    _ -> toText $ calcLS' (1000 * readDouble y) (readDouble rad) (readDouble num)
  where
    price = 0.09
    calcLS' dt pp num' = case pp of
      1 -> if dt <= 1000 then 5  * num' else 5  * num' + ((dt - 1000) * price)
      2 -> if dt <= 2000 then 10 * num' else 10 * num' + ((dt - 2000) * price)
      3 -> if dt <= 3000 then 20 * num' else 20 * num' + ((dt - 3000) * price)
      4 -> if dt <= 4000 then 40 * num' else 40 * num' + ((dt - 4000) * price)
      _ -> if dt <= 5000 then 80 * num' else 80 * num' + ((dt - 5000) * price)
      

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