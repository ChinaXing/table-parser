-- {-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module TableParser where

--import Java
import qualified Text.Parsec as P;
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Parser
import Parser (DataType(..), Index(..), Col(..))
import Data.Aeson

parse :: String -> Either P.ParseError [CreateTable]
parse ddl = P.parse a_tables "-" ddl

parseJson :: String -> String
parseJson ddl = BLU.toString . encode . parse $ ddl

--jParseJson :: JString -> Java TableParser JString
--jParseJson x = return . toJString . parseJson . fromJString $ x

--data {-# CLASS "co.callcc.TableParser" #-} TableParser = TableParser (Object# TableParser)
--foreign export java jParseJson :: JString -> Java TableParser JString

instance ToJSON P.ParseError where
  toJSON = toJSON . show

instance ToJSON DataType where
  toJSON DataType{..} = object $
    [ "type" .= t
    , "len" .= maybe (object $ [ "none" .= True ]) (\l -> object $ [ "some" .= l ]) len
    , "unsigned" .= if unsigned then
                      (object $ [ "some" .= ("UNSIGNED" :: String)])
                    else
                      (object $ [ "none" .= True ])
    ]
instance ToJSON Col where
  toJSON Col{..} = object $
    [ "id" .= idNum
    , "name" .= name
    , "dataType" .= (toJSON dataType)
    -- , "pk" .= pk
    , "autoIncrement" .= autoIncrement
    , "nullAble" .= null
    , "defaultValue" .= maybe Null (\x -> object $ maybe [ "none" .= True ] (\j -> [ "some" .= toJSON j ]) x) defaultValue
    , "updateDefaultValue" .= maybe Null toJSON updateDefaultValue
    , "comment" .= maybe Null toJSON comment
    ]

instance ToJSON Index where
  toJSON Index{..} = object $
    [ "id" .= iid
    , "name" .= iname
    , "columns" .= icolumns
    , "unique" .= unique
    , "pk" .= ipk
    , "comment" .= maybe Null toJSON icomment]
instance ToJSON CreateTable where
  toJSON CreateTable{..} = object $
    [ "indices" .= indices
    , "name" .= tableName
    , "columns" .= columns ]
