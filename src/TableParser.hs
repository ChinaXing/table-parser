-- {-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module TableParser where

--import Java
import qualified Text.Parsec as P;
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Parser
import Data.Aeson

parse :: String -> Either P.ParseError [CreateTable]
parse = P.parse aTables "-"

parseJson :: String -> String
parseJson ddl = BLU.toString . encode . parse $ ddl

--jParseJson :: JString -> Java TableParser JString
--jParseJson x = return . toJString . parseJson . fromJString $ x

--data {-# CLASS "co.callcc.TableParser" #-} TableParser = TableParser (Object# TableParser)
--foreign export java jParseJson :: JString -> Java TableParser JString

instance ToJSON P.ParseError where
  toJSON = toJSON . show

instance ToJSON DataType where
  toJSON DataType{..} = object
    [ "type" .= t
    , "len" .= maybe (object [ "none" .= True ]) (\l -> object [ "some" .= l ]) len
    , "unsigned" .= if unsigned then
                      object [ "some" .= ("UNSIGNED" :: String)]
                    else
                      object [ "none" .= True ]
    ]
instance ToJSON Col where
  toJSON Col{..} = object
    [ "id" .= id
    , "name" .= name
    , "dataType" .= toJSON dataType
    , "charset" .= maybe Null toJSON charset
    , "collate" .= maybe Null toJSON collate
    , "pk" .= pk
    , "autoIncrement" .= autoIncrement
    , "nullAble" .= nullAble
    , "defaultValue" .= maybe Null (object . maybe [ "none" .= True ] (\j -> [ "some" .= toJSON j ])) defaultValue
    , "updateDefaultValue" .= maybe Null toJSON updateDefaultValue
    , "comment" .= maybe Null toJSON comment
    ]

instance ToJSON Index where
  toJSON Index{..} = object
    [ "id" .= id
    , "name" .= name
    , "columns" .= columns
    , "unique" .= unique
    , "pk" .= pk
    , "comment" .= maybe Null toJSON comment]
instance ToJSON CreateTable where
  toJSON CreateTable{..} = object
    [ "indices" .= indices
    , "name" .= tableName
    , "columns" .= columns
    , "comment" .= comment
    , "charset" .= charset
    , "collate" .= collate
    , "engine" .= engine]
