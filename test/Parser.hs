module Parser where

import Test.Hspec
import Parser
import System.Direcotry

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "test parser create table ..." $ do
    tests <- listDirectory "./t"
    forM_ tests $ \t -> do
      (readFile t >>= parse a_createtable t) `shouldSatisfy` isRight
