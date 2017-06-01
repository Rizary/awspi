{-# LANGUAGE OverloadedStrings, RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds #-}

module Frontend.Properties.EC
  (
    ecProperties
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
-- Elastic Cache Properties
-------

calcECNode :: Int -> Text -> Text
calcECNode key y =
  case key of
    1 -> toText $ (*0.022) (readDouble y)
    2 -> toText $ (*0.052) (readDouble y)
    _ -> toText $ (*0.104) (readDouble y)

calcECUsage :: Int -> Text -> Text
calcECUsage key y =
  case key of
    1 -> toText $ (*30) (readDouble y)
    2 -> toText $ (*4)  (readDouble y)
    _ -> toText $ (*1)  (readDouble y)
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