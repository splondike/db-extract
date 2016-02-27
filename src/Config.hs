{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Config where

import Data.String(fromString)
import Data.Text(unpack)
import GHC.Generics

import Data.HashMap.Strict(foldlWithKey')
import Data.Aeson((.=), (.:))
import Data.Aeson.Types(typeMismatch, Parser)
import qualified Data.Aeson as J

data Config = Config {
   configDatabase :: DatabaseConfig,
   configReferences :: [ForeignKey]
} deriving Show

data DatabaseConfig = DatabaseConfig {
   databaseType :: String,
   databaseParams :: [(String, String)]
} deriving Show

data ForeignKey = ForeignKey 
   String -- ^ The subject table (e.g. blogs)
   String -- ^ The subject column name (e.g. author_id)
   String -- ^ The object table (e.g. authors)
   String -- ^ The object column name (e.g. id)
   deriving (Generic, Show, Ord, Eq)

-- Serialize instances
instance J.ToJSON ForeignKey
instance J.ToJSON Config where
   toJSON conf = J.object ["database" .= configDatabase conf,
                           "references" .= configReferences conf]
instance J.ToJSON DatabaseConfig where
   toJSON conf = J.object ["type" .= databaseType conf,
                           "params" .= (J.object . toAesonPairs $ databaseParams conf)]
toAesonPairs tuples = map convert tuples
   where
      convert (key, val) = (fromString key, J.toJSON val)

-- Deserialize instances
instance J.FromJSON ForeignKey
instance J.FromJSON Config where
   parseJSON (J.Object v) = Config <$> v .: "database" <*> v .: "references"
   parseJSON invalid = typeMismatch "Config" invalid
instance J.FromJSON DatabaseConfig where
   parseJSON (J.Object v) = DatabaseConfig <$> v .: "type" <*> (parseParams v)
   parseJSON invalid = typeMismatch "DatabaseConfig" invalid

parseParams obj = (obj .: "params") >>= toPairs
   where
      toPairs :: J.Object -> Parser [(String, String)]
      toPairs paramMap = handleEither $ foldlWithKey' makePair (Right []) paramMap
      makePair (Right acc) key (J.String val) = Right $ (unpack key, unpack val):acc
      makePair _ _ _ = Left "Unable to parse database params"
      handleEither (Right pairs) = return pairs
      handleEither (Left errStr) = fail errStr
