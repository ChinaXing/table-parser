module ParserSpec where

import Test.Hspec
import Parser
import System.Directory
import Text.Parsec
import Control.Monad.IO.Class
import Control.Monad
import Data.Either

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "test parser create table ..." $ do
    tests <- runIO $ listDirectory "./test/t"
    forM_ tests $ \t -> do
      result <- runIO $ (readFile ("./test/t/" ++ t) >>= return . parse a_createtable t)
      it ("parse : " ++ t) $ do result `shouldSatisfy` isRight
