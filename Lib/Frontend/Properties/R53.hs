{-# LANGUAGE OverloadedStrings, RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds #-}

module Frontend.Properties.R53
  (
    r53Properties
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
      _ -> standardQ' i
      where
       standardQ' sq'
         | sq' <= 1000 = 0.400 * sq'
	 | otherwise  = (0.400 * 1000) + (0.200 * (sq' - 1000))
	 
    latencyQ i k2 = case k2 of
      1 -> 30 * latencyQ' i
      2 -> 4 * latencyQ' i
      _ -> latencyQ' i
      where
       latencyQ' lq'
         | lq' <= 1000 = 0.600 * lq'
	 | otherwise  = (0.600 * 1000) + (0.300 * (lq' - 1000))
	 
    geoDnsQ i k3 = case k3 of
      1 -> 30 * geoDnsQ' i
      2 -> 4 * geoDnsQ' i
      _ -> geoDnsQ' i
      where
       geoDnsQ' gq'
         | gq' <= 1000 = 0.700 * gq'
	 | otherwise  = (0.700 * 1000) + (0.350 * (gq' - 1000))

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
  