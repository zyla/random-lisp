{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module SExpr where

import CustomPrelude

import qualified Data.Text as Text

data SExpr = Symbol Text | String Text | Num Integer | List [SExpr] | Vector [SExpr] | Map [SExpr] deriving (Eq, Show)

ppSExpr :: SExpr -> Text
ppSExpr = \case
  Symbol x  -> x
  String x  -> tshow x
  Num x     -> tshow x
  List xs   -> "(" <> Text.intercalate " " (map ppSExpr xs) <> ")"
  Vector xs -> "[" <> Text.intercalate " " (map ppSExpr xs) <> "]"
  Map xs    -> "{" <> Text.intercalate " " (map ppSExpr xs) <> "}"
