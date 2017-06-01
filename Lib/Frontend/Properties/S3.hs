{-# LANGUAGE OverloadedStrings, RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds #-}

module Frontend.Properties.S3
  (
    s3Properties
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
-- S3 Properties
-------

calcS3StdStore :: Text -> Int -> Text -> Text -> Text
calcS3StdStore store key pcpl go =
  toText $
    calcStore (storeKey (readDouble store) key)
              (readDouble pcpl)
	      (readDouble go)
  where
    calcStore result pcpl' go' = result + (0.000005 * pcpl') + (0.0000004 * go')
    storeKey str k1 = case k1 of
      1 -> results' str
      2 -> results' (str * 1000)
      _ -> results' (str * 1000000)
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
      _ -> (*0.01) (readDouble i)
    analytics i key = case key of
      1 -> (*3.05) (readDouble i)
      2 -> (*0.43) (readDouble i)
      _ -> (*0.10) (readDouble i)
    objectTag i key = case key of
      1 -> (*0.31 ) (readDouble i)
      2 -> (*0.05) (readDouble i)
      _ -> (*0.01) (readDouble i)
  
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