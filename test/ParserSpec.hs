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
spec =
  describe "test parser create table ..." $ do
    tests <- runIO $ listDirectory "./test/t"
    forM_ tests $ \t -> do
      result <- runIO $ liftM (parse aCreateTable t) (readFile ("./test/t/" ++ t))
      it ("parse : " ++ t) $ result `shouldSatisfy` isRight
