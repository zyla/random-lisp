{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Syntax where

import CustomPrelude

import qualified Data.Text as T
import qualified SExpr as S

newtype Ident = Ident { unIdent :: Text }
  deriving newtype (Eq, Show, Ord)

data Declaration = Declaration { ident :: Ident, type_ :: Maybe Type, body :: Expr }
  deriving (Eq, Show)

type ParseError = Text
type Parser = Either ParseError
parseError = Left

data Type
  = TyVar Ident
  | TyApp Type [Type]
  | TyFun [Type] Type
  deriving (Eq, Show)

parseType :: S.SExpr -> Parser Type
parseType = \case
  S.Symbol ident ->
    pure $ TyVar (Ident ident)
  S.List [S.Symbol "->", S.Vector args, ret] ->
    TyFun <$> traverse parseType args <*> parseType ret
  S.List (tc : args) ->
    TyApp <$> parseType tc <*> traverse parseType args
  s ->
    parseError $ "invalid type: " <> tshow s

data Expr
  = Var Ident
  | App Expr [Expr]
  | Fun [Ident] Expr
  | Lit Literal
  deriving (Eq, Show)

data Literal = IntLiteral Integer | StringLiteral Text
  deriving (Eq, Show)

parseIdent :: S.SExpr -> Parser Ident
parseIdent = \case
  S.Symbol ident ->
    pure (Ident ident)
  s ->
    parseError $ "Invalid ident: " <> tshow s

parseExpr :: S.SExpr -> Parser Expr
parseExpr = \case
  S.Symbol ident ->
    pure $ Var (Ident ident)
  S.List [S.Symbol "fn", args, ret] ->
    Fun <$> parseParameterList args <*> parseExpr ret
  S.List (fn : args) ->
    App <$> parseExpr fn <*> traverse parseExpr args
  S.Num x ->
    pure $ Lit (IntLiteral x)
  S.String x ->
    pure $ Lit (StringLiteral x)
  s ->
    parseError $ "invalid expr: " <> tshow s

parseParameterList = \case
  S.Vector args -> traverse parseIdent args
  _ -> parseError "parameterList"

parseDeclaration :: S.SExpr -> Parser Declaration
parseDeclaration = \case
  S.List [S.Symbol "def", S.Symbol ident, body] -> do
    body' <- parseExpr body
    pure Declaration
      { ident = Ident ident
      , type_ = Nothing
      , body = body'
      }

  S.List [S.Symbol "defn", S.Symbol ident, args, body] -> do
    body <- parseExpr body
    args <- parseParameterList args
    pure Declaration
      { ident = Ident ident
      , type_ = Nothing
      , body = (Fun args body)
      }

  S.List [S.Symbol "defn", S.Symbol ident, S.Symbol ":", type_, args, body] -> do
    body <- parseExpr body
    type_ <- parseType type_
    args <- parseParameterList args
    pure Declaration
      { ident = Ident ident
      , type_ = Just type_
      , body = (Fun args body)
      }

  s ->
    parseError $ "Invalid declaration: " <> tshow s

parseDeclarations :: [S.SExpr] -> Parser [Declaration]
parseDeclarations = traverse parseDeclaration
