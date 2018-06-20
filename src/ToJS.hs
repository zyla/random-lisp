{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module ToJS where

import Data.Text (Text)
import qualified Data.Text as T
import SExpr
import JS

toJS :: SExpr -> JS.Expr
toJS = \case
  SExpr.Num x -> JS.Num x
  SExpr.Symbol sym -> JS.Var (mangle sym)

  -- Special forms
  SExpr.List [SExpr.Symbol "lambda", SExpr.List args, body]
    | Just args' <- traverse fromSymbol args 
      -> JS.Function (map mangle args') (toJS body)

  SExpr.List [SExpr.Symbol "if", cond, then_, else_] ->
    JS.If (toJS cond) (toJS then_) (toJS else_)

  SExpr.List (fn:args) -> JS.Apply (toJS fn) (map toJS args)
  SExpr.List [] -> JS.ArrayLit []

fromSymbol :: SExpr -> Maybe Text
fromSymbol = \case
  Symbol x -> Just x
  _ -> Nothing

mangle :: Ident -> Text
mangle = T.concatMap $
  \case
    '~' -> "$tilde"
    '!' -> "$bang"
    '@' -> "$at"
    '$' -> "$$"
    '%' -> "$mod"
    '^' -> "$caret"
    '&' -> "$amp"
    '*' -> "$mul"
    '+' -> "$plus"
    '-' -> "$minus"
    '=' -> "$eq"
    ',' -> "$comma"
    '.' -> "$dot"
    '/' -> "$slash"
    '?' -> "$huh"
    ':' -> "$colon"
    '<' -> "$lt"
    '>' -> "$gt"
    c   -> T.singleton c
