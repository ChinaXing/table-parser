{-# LANGUAGE DuplicateRecordFields #-}
module Model where

data DataType = DataType
  { t :: String
  , len :: Maybe Int
  , unsigned :: Bool
  } deriving(Eq, Show)

data Col = Col
  { id :: Int
  , name :: String
  , dataType :: DataType
  , charset :: Maybe String
  , collate :: Maybe String
  , pk :: Bool
  , autoIncrement :: Bool
  , nullAble :: Bool
  , defaultValue :: Maybe (Maybe String)
  , updateDefaultValue :: Maybe String
  , comment :: Maybe String
  } deriving(Eq, Show)

data Index = Index
  { id :: Int
  , name :: String
  , columns :: [String]
  , unique :: Bool
  , pk :: Bool
  , comment :: Maybe String
  } deriving(Eq, Show)

data CreateTable = CreateTable
  { tableName :: String
  , columns :: [Col]
  , indices :: [Index]
  , comment :: Maybe String
  , charset :: Maybe String
  , collate :: Maybe String
  , engine :: Maybe String
  } deriving(Eq, Show)
