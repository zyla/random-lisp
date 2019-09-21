{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Text.Show.Pretty (pPrint)

import Data.Bifunctor (first)

import qualified Data.Text.IO as Text
import qualified Data.Text as Text

import qualified Parser
import qualified Syntax
import SExpr
import qualified JS
import qualified ToJS

parseS = parse Parser.sExpr ""

main :: IO ()
main = do
  hspec spec
  contents <- Text.readFile "test.clj"
  case first tshow (parse Parser.sourceFile "" contents) >>= Syntax.parseDeclarations of
    Left err ->
      Text.putStrLn err
    Right x -> do
      mapM_ pPrint x
--      Text.putStrLn $ JS.renderExpr $ ToJS.toJS x

spec :: Spec
spec = do
  describe "parsing" $ do

    it "empty list" $
      parseS "()" `shouldParse` List []

    it "non-empty list" $
      parseS "(+ 12 32)" `shouldParse` List [Symbol "+", Num 12, Num 32]

    it "non-empty list with spaces" $
      parseS " ( + 12 32 ) " `shouldParse` List [Symbol "+", Num 12, Num 32]

    it "vector" $
      parseS "[1 2 3]" `shouldParse` Vector [Num 1, Num 2, Num 3]

    it "map" $
      parseS "{:a 1 :b 2}" `shouldParse` Map [Symbol ":a", Num 1, Symbol ":b", Num 2]

    it "symbol" $
      parseS "foo" `shouldParse` Symbol "foo"

    it "weird symbol" $
      parseS "+" `shouldParse` Symbol "+"

    it "weird symbol" $
      parseS "->" `shouldParse` Symbol "->"

    it "weird symbol 2" $
      parseS "foo->bar1" `shouldParse` Symbol "foo->bar1"

    it "emojis" $
      parseS "💩\x1f397" `shouldParse` Symbol "💩\x1f397"

    it "string" $
      parseS "\"Hello\"" `shouldParse` String "Hello"

    it "string with escapes" $
      parseS "\"Hello\\\"world\"" `shouldParse` String "Hello\"world"

    it "integer" $
      parseS "1324" `shouldParse` Num 1324

tshow = Text.pack . show
