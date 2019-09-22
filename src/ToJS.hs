{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module ToJS where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isAlphaNum)
import qualified Syntax
import Text.Printf
import JS

toJS :: Syntax.Expr -> JS.Expr
toJS = \case
  Syntax.Lit (Syntax.IntLiteral x) ->
    JS.Num x

  Syntax.Var id ->
   JS.Var (mangle id)

  -- Special forms
  Syntax.Fun args body ->
    JS.Function Nothing (map (mangle . fst) args) (toJS body)

  Syntax.App fn args ->
    JS.Apply (toJS fn) (map toJS args)

mangle :: Syntax.Ident -> Text
mangle = mangle' . Syntax.unIdent

mangle' :: Text -> Text
mangle' = T.concatMap $
  \case
    c | isAlphaNum c -> T.singleton c
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
    c   -> T.pack $ printf "$u%x" (fromEnum c)
