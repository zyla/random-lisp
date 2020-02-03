{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import CustomPrelude

import Control.Monad
import Text.Show.Pretty (pPrint)
import qualified Text.Megaparsec as MP

import Data.Bifunctor (first)

import qualified Data.Text.IO as Text
import qualified Data.Text as Text

import qualified Parser
import Syntax
import SExpr (ppSExpr)
import qualified JS
import qualified ToJS
import qualified TypeCheck as TC

main :: IO ()
main = do
  ffi <- Text.readFile "example/ffi.js"
  contents <- Text.readFile "example/index.clj"

  decls <- case first (Text.pack . show) (MP.parse (Parser.sourceFile <* MP.eof) "" contents) >>= Syntax.parseDeclarations of
    Left err -> do
      error $ "Parse error:\n" <> Text.unpack err
    Right x -> pure x

  decls <-
    case TC.run $ TC.tcModule decls of
      Left err -> do
        error $ "Type error: " <> Text.unpack err
      Right x -> pure x

  Text.putStrLn ffi
             
  forM_ decls $ \case
    Declare{ident,declareType=ty} -> do
      Text.putStrLn ""
      Text.putStrLn $ "// (declare " <> unIdent ident <> " : " <> ppSExpr (serializeType ty) <> ")"
      Text.putStrLn $ "// const " <> ToJS.mangle ident <> " = undefined;"
    Def{ident,type_,body} -> do
      Text.putStrLn ""
      Text.putStr $ "// (def " <> unIdent ident
      forM_ type_ $ \ty ->
        Text.putStr $ " : " <> ppSExpr (serializeType ty)
      Text.putStrLn ""
      Text.putStrLn $ "//  " <> ppSExpr (serializeExpr body) <> ")"

      Text.putStrLn $ "var " <> ToJS.mangle ident <> " = " <> JS.renderExpr (ToJS.toJS body) <> ";"
