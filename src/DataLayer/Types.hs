module DataLayer.Types where

import qualified Data.ByteString as BS

data GetByColumnTemplate = GetByColumnTemplate {
   columnName :: String,
   tableName :: String
} deriving (Eq, Show)

data ResultRow = ResultRow {
   getRowData :: [(BS.ByteString, Maybe BS.ByteString)]
} deriving (Show, Eq, Ord)

data TableSchema = TableSchema {
   schemaName :: BS.ByteString,
   columns :: [TableColumn],
   foreignKeys :: [TableFKConstraint]
} deriving (Show)

data TableColumn = TableColumn {
   name :: BS.ByteString
} deriving (Show)

data TableFKConstraint = TableFKConstraint {
   localColumn :: BS.ByteString,
   remoteTable :: BS.ByteString,
   remoteColumn :: BS.ByteString
} deriving (Show)
