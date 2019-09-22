{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module JS where

import Data.Monoid
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

type Ident = Text

data Expr = Function (Maybe Ident) [Ident] Expr | Apply Expr [Expr] | Var Ident | String Text | Num Integer | ArrayLit [Expr] | If Expr Expr Expr
  deriving (Eq, Show)

renderExpr :: Expr -> Text
renderExpr = \case
  Function ident args body ->
    "(function " <> fromMaybe "" ident <> "(" <> T.intercalate "," args <> ") { return " <> renderExpr body <> "; })"

  Var ident ->
    ident

  Num x ->
    T.pack (show x)

  String x ->
    T.pack (show x)

  ArrayLit xs ->
    "[" <> T.intercalate "," (map renderExpr xs) <> "]"

  Apply fn args ->
    renderExpr fn <> "(" <> T.intercalate "," (map renderExpr args) <> ")"

  If cond then_ else_ ->
    "(" <> renderExpr cond <> ")?(" <> renderExpr then_ <> "):(" <> renderExpr else_ <> ")"
