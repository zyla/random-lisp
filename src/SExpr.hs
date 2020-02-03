{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module SExpr where

import CustomPrelude

import qualified Data.Text as Text

data SExpr
  = Symbol Text        -- foo
  | String Text        -- "foo"
  | Num Integer        -- 7
  | List [SExpr]       -- (a b c)
  | Vector [SExpr]     -- [a b c]
  | Map [SExpr]        -- {a b c}
  deriving (Eq, Show)

ppSExpr :: SExpr -> Text
ppSExpr = \case
  Symbol x  -> x
  String x  -> tshow x
  Num x     -> tshow x
  List xs   -> "(" <> Text.intercalate " " (map ppSExpr xs) <> ")"
  Vector xs -> "[" <> Text.intercalate " " (map ppSExpr xs) <> "]"
  Map xs    -> "{" <> Text.intercalate " " (map ppSExpr xs) <> "}"
