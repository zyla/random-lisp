{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Syntax where

import CustomPrelude

import qualified Data.Text as T
import qualified SExpr as S
import Data.Bifunctor

newtype Ident = Ident { unIdent :: Text }
  deriving newtype (Eq, Show, Ord)

data Declaration
  = Def { ident :: Ident, type_ :: Maybe Type, body :: Expr }
  | Declare { ident :: Ident, declareType :: Type }
  deriving (Eq, Show)

type ParseError = Text
type Parser = Either ParseError
parseError = Left

data Type
  = TyVar Ident
  | TyApp Type [Type]
  | TyFun [Type] Type
  | TyForall [Ident] Type
  deriving (Eq, Show)

parseType :: S.SExpr -> Parser Type
parseType = \case
  S.Symbol ident ->
    pure $ TyVar (Ident ident)
  S.List [S.Symbol "->", S.Vector args, ret] ->
    TyFun <$> traverse parseType args <*> parseType ret
  S.List [S.Symbol "forall", S.Vector args, ret] ->
    TyForall <$> traverse parseIdent args <*> parseType ret
  S.List (tc : args) ->
    TyApp <$> parseType tc <*> traverse parseType args
  s ->
    parseError $ "invalid type: " <> tshow s

serializeType :: Type -> S.SExpr
serializeType = \case
  TyVar (Ident i) ->
    S.Symbol i
  TyFun args ret ->
    S.List [S.Symbol "->", S.Vector (serializeType <$> args), serializeType ret]
  TyForall tvs ret ->
    S.List [S.Symbol "forall", S.Vector (serializeIdent <$> tvs), serializeType ret]
  TyApp tc args ->
    S.List (serializeType tc : (serializeType <$> args))

data Expr
  = Var Ident
  | App Expr [Expr]
  | Fun [(Ident, Type)] Expr
  | Lit Literal
  | Block [Expr]
  | Let [(Ident, Expr)] Expr
  deriving (Eq, Show)

data Literal = IntLiteral Integer | StringLiteral Text
  deriving (Eq, Show)

parseIdent :: S.SExpr -> Parser Ident
parseIdent = \case
  S.Symbol ident ->
    pure (Ident ident)
  s ->
    parseError $ "Invalid ident: " <> tshow s

serializeIdent (Ident i) = S.Symbol i

parseExpr :: S.SExpr -> Parser Expr
parseExpr = \case
  S.Symbol ident ->
    pure $ Var (Ident ident)
  S.List [S.Symbol "fn", args, ret] ->
    Fun <$> parseParameterList args <*> parseExpr ret
  S.List (S.Symbol "do" : exprs) ->
    makeBlock <$> traverse parseExpr exprs
  S.List (S.Symbol "let" : S.Vector binders : exprs) ->
    Let
      <$> traverse parseLetBinder binders
      <*> (makeBlock <$> traverse parseExpr exprs)
  S.List (fn : args) ->
    App <$> parseExpr fn <*> traverse parseExpr args
  S.Num x ->
    pure $ Lit (IntLiteral x)
  S.String x ->
    pure $ Lit (StringLiteral x)
  s ->
    parseError $ "invalid expr: " <> S.ppSExpr s

parseLetBinder :: S.SExpr -> Parser (Ident, Expr)
parseLetBinder = \case
  S.List [S.Symbol ident, expr] ->
    (,) (Ident ident) <$> parseExpr expr
  s ->
    parseError $ "invalid let binder: " <> S.ppSExpr s

makeBlock [x] = x
makeBlock xs = Block xs

serializeExpr :: Expr -> S.SExpr
serializeExpr = \case
  Var (Ident ident) ->
    S.Symbol ident
  Fun args ret ->
    S.List [S.Symbol "fn", S.List ((\(ident, ty) -> S.List [serializeIdent ident, serializeType ty]) <$> args), serializeExpr ret]
  Block exprs ->
    S.List (S.Symbol "do" : (serializeExpr <$> exprs))
  Let binders body -> 
    S.List [S.Symbol "let", S.Vector (serializeLetBinder <$> binders), serializeExpr body]
  App fn args ->
    S.List (serializeExpr fn : (serializeExpr <$> args))
  Lit (IntLiteral x) ->
    S.Num x
  Lit (StringLiteral x) ->
    S.String x
  expr ->
    terror $ "serializeExpr: unhandled " <> tshow expr

serializeLetBinder (ident, expr) = S.List [serializeIdent ident, serializeExpr expr]

ppExpr = S.ppSExpr . serializeExpr
ppType = S.ppSExpr . serializeType

parseParameterList = \case
  S.Vector args -> traverse parseParam args
  _ -> parseError "parameterList"

parseParam = \case
  S.List [id, ty] -> do
    ident <- parseIdent id
    type_ <- parseType ty
    pure (ident, type_)
  _ -> parseError "param"

parseDeclaration :: S.SExpr -> Parser Declaration
parseDeclaration = \case
  S.List [S.Symbol "def", S.Symbol ident, body] -> do
    body' <- parseExpr body
    pure Def
      { ident = Ident ident
      , type_ = Nothing
      , body = body'
      }

  S.List [S.Symbol "defn", S.Symbol ident, args, body] -> do
    body <- parseExpr body
    args <- parseParameterList args
    pure Def
      { ident = Ident ident
      , type_ = Nothing
      , body = (Fun args body)
      }

  S.List [S.Symbol "defn", S.Symbol ident, S.Symbol ":", type_, args, body] -> do
    body <- parseExpr body
    type_ <- parseType type_
    args <- parseParameterList args
    pure Def
      { ident = Ident ident
      , type_ = Just type_
      , body = (Fun args body)
      }

  S.List [S.Symbol "declare", S.Symbol ident, S.Symbol ":", type_] -> do
    type_ <- parseType type_
    pure Declare
      { ident = Ident ident
      , declareType = type_
      }

  s ->
    parseError $ "Invalid declaration: " <> tshow s

parseDeclarations :: [S.SExpr] -> Parser [Declaration]
parseDeclarations = traverse parseDeclaration
