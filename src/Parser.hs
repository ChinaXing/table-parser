{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}

module Parser (a_createtable, a_tables, DataType(..), Col(..), Index(..), CreateTable(..)) where

import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec.Char
import Data.Maybe
import Data.Char as C
import Data.Either
import Control.Monad
import qualified Data.List as DL
import qualified Control.Applicative as CA (Alternative(empty), (<|>))



data DataType = DataType
  { t :: String
  , len :: Maybe Int
  , unsigned :: Bool
  } deriving(Eq, Show)

data Col = Col
  { idNum :: Int
  , name :: String
  , dataType :: DataType
  , pk :: Bool
  , autoIncrement :: Bool
  , null :: Bool
  , defaultValue :: Maybe (Maybe String)
  , updateDefaultValue :: Maybe String
  , comment :: Maybe String
  } deriving(Eq, Show)

data Index = Index
  { iid :: Int
  , iname :: String
  , icolumns :: [String]
  , unique :: Bool
  , ipk :: Bool
  , icomment :: Maybe String
  } deriving(Eq, Show)

data CreateTable = CreateTable
  { tableName :: String
  , columns :: [Col]
  , indices :: [Index]
  } deriving(Eq, Show)

--createTable :: GenParser Char st CreateTable

string_ci :: (Stream s m Char) => String -> ParsecT s u m String
string_ci s = foldM (\r c -> satisfy (\c' -> c' == c || (toUpper c' == toUpper c)) >>= return . (:r))
                [] s >>= return . reverse

string_ci_ :: (Stream s m Char) => String -> ParsecT s u m ()
string_ci_ = foldM (\() c -> satisfy (\c' -> c' == c || (toUpper c' == toUpper c)) >> return ()) ()

qt :: (Stream s m Char) => Char -> Char -> ParsecT s u m a -> ParsecT s u m [a]
qt l r p = char l *> manyTill p (char r)

paren_between :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
paren_between = between (char '(') (char ')')

str_qt :: Stream s m Char => ParsecT s u m a -> ParsecT s u m [a]
str_qt p = single_qt p <|> qt '"' '"' p

choice' :: Stream s m  Char => [ParsecT s u m a] -> ParsecT s u m a
choice' = foldl (\r i -> r <|> try i) CA.empty

qt_optional :: Stream s m Char => Char -> Char -> ParsecT s u m a -> ParsecT s u m [a]
qt_optional l r p = (qt l r p) <|> manyTill p (lookAhead $ oneOf "\n,) ")

single_qt :: Stream s m Char => ParsecT s u m a -> ParsecT s u m [a]
single_qt  = qt '\'' '\''

single_qt_optional :: Stream s m Char => ParsecT s u m a -> ParsecT s u m [a]
single_qt_optional p = single_qt p <|> manyTill p (lookAhead $ oneOf "\n,) ")

str_qt_optional :: Stream s m Char => ParsecT s u m a -> ParsecT s u m [a]
str_qt_optional p = str_qt p <|> manyTill p (lookAhead $ oneOf "\n,) ")

paren_qt :: Stream s m Char => ParsecT s u m a -> ParsecT s u m [a]
paren_qt = qt '(' ')'

back_qt :: Stream s m Char => ParsecT s u m a -> ParsecT s u m [a]
back_qt = qt '`' '`'

back_qt_optional :: Stream s m Char => ParsecT s u m a  -> ParsecT s u m [a]
back_qt_optional p = qt '`' '`' p <|> manyTill p (lookAhead $ oneOf "\n,) ")

a_datatype_optional_length :: (Stream s m Char) => String -> ParsecT s u m String
a_datatype_optional_length ts = string_ci ts <* optional (paren_qt digit)

a_inttype :: (Stream s m Char) => String -> ParsecT s u m DataType
a_inttype ts = do
   t <- a_datatype_optional_length ts
   u <- optionMaybe $ try (spaces *> string_ci_ "UNSIGNED")
   return $ DataType { t = ts, len = Nothing, unsigned = (isJust u)}

a_decimal :: (Stream s m Char) => ParsecT s u m DataType
a_decimal = do
  string_ci "DECIMAL"
  optional $ try $ paren_qt $ char ',' <|> digit
  u <- optionMaybe $ try (spaces *> string_ci "UNSIGNED")
  return $ DataType { t = "DECIMAL", len = Nothing, unsigned = (isJust u)}

a_chartype :: (Stream s m Char) => String -> ParsecT s u m DataType
a_chartype ts = do
  t <- string_ci ts
  len <- paren_qt  digit
  return $ DataType { t = ts, len = (Just $ read len), unsigned = False }

a_texttype :: (Stream s m Char) => String -> ParsecT s u m DataType
a_texttype ts = do
  t <- string_ci ts
  return $ DataType { t = ts, len = Nothing, unsigned = False }

a_blobtype :: (Stream s m Char) => String -> ParsecT s u m DataType
a_blobtype ts = do
  t <- string_ci ts
  return $ DataType { t = ts, len = Nothing, unsigned = False }

a_enumtype :: (Stream s m Char) => ParsecT s u m DataType
a_enumtype = do
  string_ci "ENUM"
  paren_between $ (spaces *> single_qt anyChar <* (optional spaces)) `sepBy` (char ',')
  return $ DataType { t = "VARCHAR", len = Just 256, unsigned = False }

a_datetimetype :: (Stream s m Char) => ParsecT s u m DataType
a_datetimetype = do
  string_ci_ "DATETIME"
  optional $ try $ paren_qt digit
  return $ DataType { t = "DATETIME", len = Nothing, unsigned = False }

a_yeartype :: (Stream s m Char) => ParsecT s u m DataType
a_yeartype = do
  string_ci_ "YEAR"
  optional $ try $ paren_qt digit
  return $ DataType { t = "YEAR", len = Nothing, unsigned = False }

a_floattype :: (Stream s m Char) => String -> ParsecT s u m DataType
a_floattype ts = do
  t <- string_ci ts
  optional $ try $ spaces *> char '(' *> many digit *> char ',' *> many digit *> char ')'
  u <- optionMaybe $ try (spaces *> string_ci "UNSIGNED")
  return $ DataType { t = ts, len = Nothing, unsigned = isJust u }

a_datetype :: (Stream s m Char) => ParsecT s u m DataType
a_datetype = do
  string_ci_ "DATE"
  return $ DataType { t = "DATE", len = Nothing, unsigned = False }

a_timestamptype :: (Stream s m Char) => ParsecT s u m DataType
a_timestamptype = do
  string_ci "TIMESTAMP"
  return $ DataType { t = "TIMESTAMP", len = Nothing, unsigned = False }

a_dataType :: (Stream s m Char) => ParsecT s u m DataType
a_dataType = choice' [ a_inttype "INT"
                    , a_inttype "BIGINT"
                    , a_inttype "SMALLINT"
                    , a_inttype "TINYINT"
                    , a_inttype "MEDIUMINT"
                    , a_inttype "BIT"
                    , a_decimal
                    , a_floattype "FLOAT"
                    , a_floattype "DOUBLE"
                    , a_chartype "CHAR"
                    , a_chartype "VARCHAR"
                    , a_chartype "BINARY"
                    , a_chartype "VARBINARY"
                    , a_texttype "TEXT"
                    , a_texttype "TINYTEXT"
                    , a_texttype "MEDIUMTEXT"
                    , a_texttype "LONGTEXT"
                    , a_blobtype "BLOB"
                    , a_blobtype "MEDIUMBLOB"
                    , a_blobtype "LONGBLOB"
                    , a_enumtype
                    , a_datetimetype
                    , a_datetype
                    , a_timestamptype
                    , a_yeartype]

a_columnname :: (Stream s m Char) => ParsecT s u m String
a_columnname =  notFollowedBy (pk_prefix <|> pindex_prefix) *>
                  (back_qt_optional $ alphaNum <|> char '_' <|> char ' ')

a_nullable :: (Stream s m Char) => ParsecT s u m Bool
a_nullable = do
  s <- spaces *> ((try $ string_ci "NOT NULL") <|> (try $ string_ci "NULL"))
  return (map toUpper s /= "NOT NULL")

a_defaultvalue :: (Stream s m Char) => ParsecT s u m (Maybe String)
a_defaultvalue =  do
  space *> (string_ci_ "DEFAULT") *> space
  ((try $ string_ci_ "NULL") *> return Nothing) <|> (str_qt_optional anyChar >>= return . Just)

a_updatedefaultvalue :: (Stream s m Char) => ParsecT s u m String
a_updatedefaultvalue = space *> string_ci_ "ON" *> many1 space *> string_ci_ "UPDATE" *> space *> str_qt_optional anyChar

a_comment :: (Stream s m Char) => ParsecT s u m String
a_comment = space *> string_ci_ "COMMENT" *> space *> str_qt anyChar

a_autoincrement :: (Stream s m Char) => ParsecT s u m Bool
a_autoincrement = space *> string_ci_ "AUTO_INCREMENT" *> return True

a_pk :: (Stream s m Char) => ParsecT s u m Bool
a_pk = space *> string_ci_ "PRIMARY" *> many1 space *> string_ci_ "KEY" *> return True

a_collate :: (Stream s m Char) => ParsecT s u m String
a_collate = do
  many1 space
  string_ci_ "COLLATE"
  many1 space
  many (alphaNum <|> char '_')

a_charset :: (Stream s m Char) => ParsecT s u m String
a_charset = do
  many1 space
  string_ci_ "CHARACTER SET"
  many1 space
  many (alphaNum <|> char '_')

a_column :: (Stream s m Char) => ParsecT s u m Col
a_column = do
  name <- a_columnname
  spaces
  dt <- a_dataType
  optional $ try a_charset
  optional $ try a_collate
  optional $ try (spaces *> string "zerofill")
  nullable1 <- optionMaybe  $ try a_nullable
  defaultValue <-  optionMaybe $ try a_defaultvalue
  updateDefault <- optionMaybe $ try a_updatedefaultvalue
  pk <- option False $ try a_pk
  autoIncrement <- option False $ try a_autoincrement
  nullable2 <- optionMaybe $ try a_nullable
  comment <- optionMaybe $ try a_comment
  let nullable = maybe True Prelude.id $ nullable1 CA.<|> nullable2
  return $ Col { idNum = 2
               , name = name
               , dataType = dt
               , pk = pk
               , autoIncrement = autoIncrement
               , null = nullable
               , defaultValue = defaultValue
               , updateDefaultValue = updateDefault
               , comment = comment
               }

data KeyType = PK | UNIQUE_KEY | KEY deriving(Eq, Show)

pk_prefix :: Stream s m Char => ParsecT s u m KeyType
pk_prefix = string_ci_ "PRIMARY" *> many1 space *> string_ci_ "KEY" *> lookAhead space *> pure PK

a_pkindex :: Stream s m Char => ParsecT s u m (KeyType, String)
a_pkindex = pk_prefix *> pure (PK, "PK")

pindex_prefix :: Stream s m Char => ParsecT s u m KeyType
pindex_prefix = do
  u <- optionMaybe $ (string_ci "UNIQUE" <|> string_ci "FULLTEXT") <* many1 space
  string_ci_ "KEY" <|> string_ci_ "INDEX"
  many1 space
  return (if (isJust u) then UNIQUE_KEY else KEY)

a_plainindex :: Stream s m Char => ParsecT s u m (KeyType, String)
a_plainindex = do
  kt <- pindex_prefix
  name <- back_qt_optional $ alphaNum <|> char '_' <|> char '+' <|> char '-'
  return $ (kt, name)

a_constraint :: Stream s m Char => ParsecT s u m ()
a_constraint = do
  string_ci_ "CONSTRAINT "
  spaces
  manyTill anyChar (lookAhead $ char ',' <|> char '\n')
  return ()

a_index :: Stream s m Char => ParsecT s u m Index
a_index = do
  (kt, name) <- a_pkindex <|> a_plainindex
  many1 space
  columns <- paren_between $
    ((back_qt $ alphaNum <|> char '_') <* (optional $ paren_qt alphaNum)) `sepBy` (char ',' *> spaces)
  optionMaybe $ try (spaces *> string_ci_ "USING " *> (string_ci_ "BTREE" <|> string_ci_ "HASH"))
  comment <- optionMaybe $
    try $ space *> string_ci_ "COMMENT" *> many1 space *> str_qt anyChar
  return $ Index { iid = 1
                 , iname = name
                 , icolumns = columns
                 , unique = (kt == UNIQUE_KEY)
                 , ipk = kt == PK
                 , icomment = comment
                 }

a_createtable :: Stream s m Char => ParsecT s u m CreateTable
a_createtable = do
  string_ci_ "CREATE" *> many1 space *> string_ci_ "TABLE"
  many1 space
  tablename <- back_qt_optional $ alphaNum <|> char '_'
  spaces
  (indices, cols) <- paren_between $ nl_space_around *> column_index_defs <* nl_space_around
  manyTill anyChar (eof <|> lookAhead (char ';' >> return ()))
  return $ CreateTable { tableName = tablename
                       , columns = setColIdNum cols
                       , indices = setIndexIdNum $ filter (\Index{..} -> iname /= "__X__") indices }
  where
    setColIdNum = map (\(a, b) -> b { idNum = a } :: Col) . zip [1..]
    setIndexIdNum = map (\(a, b) -> b { iid = a } :: Index) . zip [1..]
    skipIndex = Left $
      Index { iid = 0, iname = "__X__", icolumns = []
            , unique = False, ipk = False, icomment = Nothing }
    column_index_defs = (column_or_index `sepBy` seperator) >>= return . partitionEithers
    column_or_index = (try a_constraint *> return skipIndex)
                      <|> (try a_index >>= return . Left)
                      <|> (a_column >>= return . Right)
    seperator = try (spaces *> (char ',') *> spaces *> (optional $ char '\n') *> spaces)
    nl_space_around = spaces *> (optional $ char '\n') *> spaces

a_tables :: Stream s m Char => ParsecT s u m [CreateTable]
a_tables = a_createtable `endBy` (eof <|> (char ';' *> optional (char '\n')))
