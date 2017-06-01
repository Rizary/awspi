{-# LANGUAGE OverloadedStrings, RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds #-}

module Frontend.Properties.DDB
  (
    dbProperties
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
-- DynamoDB Properties
-------

calcDBData :: Text -> Int -> Text
calcDBData cmv ckey = toText $
  case ckey of
    1 -> calcDB' $ readDouble cmv
    _ -> calcDB' $ 1000 * (readDouble cmv)

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