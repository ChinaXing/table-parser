{-# LANGUAGE RecordWildCards #-}
module ParserSpec where

import Test.Hspec
import Parser
import Model
import System.Directory
import Text.Parsec
import Control.Monad.IO.Class
import Control.Monad
import Data.Either

main :: IO ()
main = hspec spec

tableCols :: CreateTable -> [Col]
tableCols CreateTable{..} = columns

spec :: Spec
spec = do
  describe "parser create table ..." $ do
    tests <- runIO $ listDirectory "./test/t"
    forM_ tests $ \t -> do
      result <- runIO $ liftM (parse aCreateTable t) (readFile ("./test/t/" ++ t))
      it ("parse : " ++ t) $ result `shouldSatisfy` isRight
  describe "parse create table special case" $ do
    let file =  "./test/t/t10.sql"
    sql <- runIO $ readFile file
    let result = parse aCreateTable file sql
    it ("parse : " ++ file) $ result `shouldSatisfy` isRight
    let (Right table) = result
        cols = tableCols table
        Col{..} = head $ filter (\Col{..} -> name == "sub_title") cols
    it "default value's singleQuote escape" $ defaultValue `shouldBe` Just (Just "'")
    it "comment 's single escpae" $ comment `shouldBe` Just "子标'题"
