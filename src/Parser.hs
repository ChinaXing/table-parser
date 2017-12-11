{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Parser (aCreateTable, aTables, DataType(..), Col(..), Index(..), CreateTable(..)) where

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

--createTable :: GenParser Char st CreateTable

stopChars :: (Stream s m Char) => ParsecT s u m Char
stopChars = char ' ' <|> char '\n' <|> char ',' <|> char ')' <|> char '('

stringCi :: (Stream s m Char) => String -> ParsecT s u m String
stringCi s = liftM reverse (foldM (\r c -> liftM (:r) (satisfy (\c' -> c' == c || (toUpper c' == toUpper c)))) [] s)

stringCi_ :: (Stream s m Char) => String -> ParsecT s u m ()
stringCi_ = foldM (\() c -> void (satisfy (\c' -> c' == c || (toUpper c' == toUpper c)))) ()

escape :: (Stream s m Char) => ParsecT s u m (Maybe Char)
escape = optionMaybe (try (char '\'' <* lookAhead (char '\'')))

qt :: (Stream s m Char) => Char -> Char -> ParsecT s u m a -> ParsecT s u m [a]
qt l r p = char l *> scan <* char r
  where
    scan = do
      esc <- escape
      case esc of
        Nothing -> (lookAhead (char r) *> return []) <|> ((:) <$> p <*> scan)
        Just _ -> (:) <$> p <*> scan

parenBetween :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
parenBetween = between (char '(') (char ')')

strQuote :: Stream s m Char => ParsecT s u m a -> ParsecT s u m [a]
strQuote p = singleQuote p <|> qt '"' '"' p

choice' :: Stream s m  Char => [ParsecT s u m a] -> ParsecT s u m a
choice' = foldl (\r i -> r <|> try i) CA.empty

quoteOptional :: Stream s m Char => Char -> Char -> ParsecT s u m a -> ParsecT s u m [a]
quoteOptional l r p = qt l r p <|> manyTill p (lookAhead stopChars)

singleQuote :: Stream s m Char => ParsecT s u m a -> ParsecT s u m [a]
singleQuote  = qt '\'' '\''

singleQuoteOptional :: Stream s m Char => ParsecT s u m a -> ParsecT s u m [a]
singleQuoteOptional p = singleQuote p <|> manyTill p (lookAhead stopChars)

strQuoteOptional :: Stream s m Char => ParsecT s u m a -> ParsecT s u m [a]
strQuoteOptional p = strQuote p <|> manyTill p (lookAhead stopChars)

parenQuote :: Stream s m Char => ParsecT s u m a -> ParsecT s u m [a]
parenQuote = qt '(' ')'

backQuote :: Stream s m Char => ParsecT s u m a -> ParsecT s u m [a]
backQuote = qt '`' '`'

backQuoteOptional :: Stream s m Char => ParsecT s u m a  -> ParsecT s u m [a]
backQuoteOptional p = qt '`' '`' p <|> manyTill p (lookAhead stopChars)

aDataTypeOptionalLength :: (Stream s m Char) => String -> ParsecT s u m String
aDataTypeOptionalLength ts = stringCi ts <* optional (parenQuote digit)

aIntType :: (Stream s m Char) => String -> ParsecT s u m DataType
aIntType ts = do
   t <- aDataTypeOptionalLength ts
   u <- optionMaybe $ try (many1 space *> stringCi_ "UNSIGNED")
   return DataType { t = ts, len = Nothing, unsigned = isJust u}

aDecimal :: (Stream s m Char) => ParsecT s u m DataType
aDecimal = do
  stringCi "DECIMAL"
  optional $ try $ parenQuote $ char ',' <|> digit
  u <- optionMaybe $ try (many1 space *> stringCi "UNSIGNED")
  return DataType { t = "DECIMAL", len = Nothing, unsigned = isJust u}

aCharType :: (Stream s m Char) => String -> ParsecT s u m DataType
aCharType ts = do
  t <- stringCi ts
  len <- parenQuote digit
  return DataType { t = ts, len = Just $ read len, unsigned = False }

aTextType :: (Stream s m Char) => String -> ParsecT s u m DataType
aTextType ts = do
  t <- stringCi ts
  return DataType { t = ts, len = Nothing, unsigned = False }

aBlobType :: (Stream s m Char) => String -> ParsecT s u m DataType
aBlobType ts = do
  t <- stringCi ts
  return DataType { t = ts, len = Nothing, unsigned = False }

aEnumType :: (Stream s m Char) => ParsecT s u m DataType
aEnumType = do
  stringCi "ENUM"
  parenBetween $ (many1 space *> singleQuote anyChar <* optional spaces) `sepBy` char ','
  return DataType { t = "VARCHAR", len = Just 256, unsigned = False }

aDateTimeType :: (Stream s m Char) => ParsecT s u m DataType
aDateTimeType = do
  stringCi_ "DATETIME"
  optional $ try $ parenQuote digit
  return DataType { t = "DATETIME", len = Nothing, unsigned = False }

aYearType :: (Stream s m Char) => ParsecT s u m DataType
aYearType = do
  stringCi_ "YEAR"
  optional $ try (parenQuote digit)
  return DataType { t = "YEAR", len = Nothing, unsigned = False }

aFloatType :: (Stream s m Char) => String -> ParsecT s u m DataType
aFloatType ts = do
  t <- stringCi ts
  optional $ try $ spaces *> char '(' *> many digit *> char ',' *> many digit *> char ')'
  u <- optionMaybe $ try (many1 space *> stringCi "UNSIGNED")
  return DataType { t = ts, len = Nothing, unsigned = isJust u }

aDateType :: (Stream s m Char) => ParsecT s u m DataType
aDateType = do
  stringCi_ "DATE"
  return DataType { t = "DATE", len = Nothing, unsigned = False }

aTimeType :: (Stream s m Char) => ParsecT s u m DataType
aTimeType = do
  stringCi_ "TIME"
  return DataType { t = "TIME", len = Nothing, unsigned = False }

aTimestampType :: (Stream s m Char) => ParsecT s u m DataType
aTimestampType = do
  stringCi "TIMESTAMP"
  return DataType { t = "TIMESTAMP", len = Nothing, unsigned = False }

aDataType :: (Stream s m Char) => ParsecT s u m DataType
aDataType = choice' [ aIntType "INT"
                    , aIntType "BIGINT"
                    , aIntType "SMALLINT"
                    , aIntType "TINYINT"
                    , aIntType "MEDIUMINT"
                    , aIntType "BIT"
                    , aDecimal
                    , aFloatType "FLOAT"
                    , aFloatType "DOUBLE"
                    , aCharType "CHAR"
                    , aCharType "VARCHAR"
                    , aCharType "BINARY"
                    , aCharType "VARBINARY"
                    , aTextType "TEXT"
                    , aTextType "TINYTEXT"
                    , aTextType "MEDIUMTEXT"
                    , aTextType "LONGTEXT"
                    , aBlobType "BLOB"
                    , aBlobType "MEDIUMBLOB"
                    , aBlobType "LONGBLOB"
                    , aEnumType
                    , aDateTimeType
                    , aDateType
                    , aTimestampType
                    , aTimeType
                    , aYearType]

aColumnname :: (Stream s m Char) => ParsecT s u m String
aColumnname =  notFollowedBy (aPkPrefix <|> aPlainIndexPrefix) *>
                  backQuoteOptional (alphaNum <|> char '_' <|> char ' ')

aNullable :: (Stream s m Char) => ParsecT s u m Bool
aNullable = try (a_not_null >> return False) <|> try (a_null >> return True)
  where
    a_not_null = stringCi "NOT" *> many1 space *> stringCi "NULL"
    a_null = stringCi "NULL"

aDefaultValue :: (Stream s m Char) => ParsecT s u m (Maybe String)
aDefaultValue =  do
  stringCi_ "DEFAULT" *> many1 space
  (try (stringCi_ "NULL") *> return Nothing) <|> liftM Just (strQuoteOptional anyChar)

aUpdateDefaultValue :: (Stream s m Char) => ParsecT s u m String
aUpdateDefaultValue = stringCi_ "ON" *> many1 space *> stringCi_ "UPDATE" *> space *> strQuoteOptional anyChar

aComment :: (Stream s m Char) => ParsecT s u m String
aComment = stringCi_ "COMMENT" *> many1 space *> strQuote anyChar

aAutoincrement :: (Stream s m Char) => ParsecT s u m Bool
aAutoincrement = stringCi_ "AUTO_INCREMENT" *> return True

aPk :: (Stream s m Char) => ParsecT s u m Bool
aPk = stringCi_ "PRIMARY" *> many1 space *> stringCi_ "KEY" *> return True

aCollate :: (Stream s m Char) => Maybe String -> ParsecT s u m String
aCollate charset = do
  stringCi_ "COLLATE"
  many1 space
  cs <- maybe (many1 alphaNum) string charset
  char '_'
  b <- many1 (alphaNum <|> char '_')
  return $ cs ++ "_" ++ b

aCharset :: (Stream s m Char) => ParsecT s u m String
aCharset = do
  stringCi_ "CHARACTER"
  many1 space
  stringCi_ "SET"
  many1 space
  many1 (alphaNum <|> char '_')

aColumn :: (Stream s m Char) => ParsecT s u m Col
aColumn = do
  name <- aColumnname
  many1 space
  dataType <- aDataType
  charset <- optionMaybe $ try (many1 space *> aCharset)
  collate <- optionMaybe $ try (many1 space *> aCollate charset)
  optional $ try (many1 space *> string "zerofill")
  nullable1 <- optionMaybe  $ try (many1 space *> aNullable)
  defaultValue <-  optionMaybe $ try (many1 space *> aDefaultValue)
  updateDefaultValue <- optionMaybe $ try (many1 space *> aUpdateDefaultValue)
  pk <- option False $ try (many1 space *> aPk)
  autoIncrement <- option False $ try (many1 space *> aAutoincrement)
  nullable2 <- optionMaybe $ try (many1 space *> aNullable)
  comment <- optionMaybe $ try (many1 space *> aComment)
  let nullAble = fromMaybe True $ nullable1 CA.<|> nullable2
      id = 0
  return Col{..}

data KeyType = PK | UNIQUE_KEY | KEY deriving(Eq, Show)

aPkPrefix :: Stream s m Char => ParsecT s u m KeyType
aPkPrefix = stringCi_ "PRIMARY" *> many1 space *> stringCi_ "KEY" *> lookAhead space *> pure PK

aPkIndex :: Stream s m Char => ParsecT s u m (KeyType, String)
aPkIndex = aPkPrefix *> pure (PK, "PK")

aPlainIndexPrefix :: Stream s m Char => ParsecT s u m KeyType
aPlainIndexPrefix = do
  u <- optionMaybe $ (stringCi "UNIQUE" <|> stringCi "FULLTEXT") <* many1 space
  optional $ (stringCi_ "KEY" <|> stringCi_ "INDEX") <* many1 space
  return $ maybe KEY (const UNIQUE_KEY) u

aPlainIndex :: Stream s m Char => ParsecT s u m (KeyType, String)
aPlainIndex = do
  kt <- aPlainIndexPrefix
  name <- backQuoteOptional $ alphaNum <|> char '_' <|> char '+' <|> char '-'
  return (kt, name)

aConstraint :: Stream s m Char => ParsecT s u m ()
aConstraint = do
  stringCi_ "CONSTRAINT "
  many1 space
  manyTill anyChar (lookAhead $ char ',' <|> char '\n')
  return ()

aIndex :: Stream s m Char => ParsecT s u m Index
aIndex = do
  (kt, name) <- aPkIndex <|> aPlainIndex
  spaces
  columns <- parenBetween $
    ((backQuote $ alphaNum <|> char '_') <* (optional $ parenQuote alphaNum)) `sepBy` (char ',' *> spaces)
  optionMaybe $ try (spaces *> stringCi_ "USING" *> many1 space*> (stringCi_ "BTREE" <|> stringCi_ "HASH"))
  comment <- optionMaybe $
    try $ many1 space *> stringCi_ "COMMENT" *> many1 space *> strQuote anyChar
  let unique = kt == UNIQUE_KEY
      id = 0
      pk = kt == PK
  return Index{..}

aKey :: Stream s m Char => String -> ParsecT s u m String
aKey k = stringCi k <*  spaces <*  char '=' <* spaces

aTableCharset :: Stream s m Char => ParsecT s u m String
aTableCharset = do
  optional $ try (stringCi_ "DEFAULT" *> many1 space)
  aKey "CHARSET"
  many1 alphaNum

aCreateTable :: Stream s m Char => ParsecT s u m CreateTable
aCreateTable = do
  stringCi_ "CREATE" *> many1 space *> stringCi_ "TABLE"
  many1 space
  tableName <- backQuoteOptional $ alphaNum <|> char '_'
  spaces
  (columns, indices) <- parenBetween $ nl_space_around *> column_index_defs <* nl_space_around
  spaces
  engine <- optionMaybe $ try (aKey "ENGINE" *> many1 alphaNum)
  optional $ try (many1 space *> aKey "AUTO_INCREMENT" *> many1 digit)
  charset <- optionMaybe $ try (many1 space *> aTableCharset)
  collate <- optionMaybe $ try $ do
    many1 space
    aKey "COLLATE"
    cs <- maybe (many1 alphaNum) stringCi charset
    char '_'
    co <- many1 alphaNum
    return $ cs ++ "_" ++ co
  manyTill anyChar (eof <|> lookAhead (void (char ';') <|> try (void (aKey "COMMENT"))))
  comment <- optionMaybe $ try (aKey "COMMENT" *> strQuote anyChar)
  manyTill anyChar (eof <|> lookAhead (void (char ';')))
  return CreateTable{..}
  where
    column_index_defs = liftM (fmap catMaybes . partitionEithers) (column_or_index `sepBy` seperator)
    column_or_index = (try aConstraint >> return (Right Nothing))
                      <|> liftM (Right . Just) (try aIndex)
                      <|> liftM Left aColumn
    seperator = try (spaces *> char ',' *> spaces *> optional (char '\n') *> spaces)
    nl_space_around = spaces *> optional (char '\n') *> spaces

aTables :: Stream s m Char => ParsecT s u m [CreateTable]
aTables = aCreateTable `endBy` (eof <|> (char ';' *> optional (char '\n')))
