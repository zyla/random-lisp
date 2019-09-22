{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NamedFieldPuns #-}

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
import qualified JS
import qualified ToJS
import qualified TypeCheck as TC

main :: IO ()
main = do
  contents <- Text.readFile "test.clj"
  decls <- case first tshow (MP.parse (Parser.sourceFile <* MP.eof) "" contents) >>= Syntax.parseDeclarations of
    Left err -> do
      Text.putStrLn err
      error "err"
    Right x -> pure x

  decls <-
    case TC.run $ TC.tcModule decls of
      Left err -> do
        Text.putStrLn err
        error "err"
      Right x -> pure x
             
  forM_ decls $ \decl -> do
    pPrint decl
