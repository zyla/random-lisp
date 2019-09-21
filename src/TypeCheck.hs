{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module TypeCheck where

import CustomPrelude
import qualified Data.Map as Map

import Syntax

type Context = Map Ident Type

lookup :: Ident -> Context -> TC Type
lookup id ctx = maybe (Left ("undefined : " <> tshow id)) pure $ Map.lookup id ctx

type TC = Either Text

err = Left

infer :: Context -> Expr -> TC Type
infer ctx = \case

  Var id ->
    lookup id ctx

  Lit (IntLiteral _) ->
    pure (TyVar (Ident "Int"))

  Lit (StringLiteral _) ->
    pure (TyVar (Ident "String"))

  App fn args -> do
    fnty <- infer ctx fn
    case fnty of
      TyFun argtys retty -> do
        argtysInferred <- traverse (infer ctx) args
        pure retty
      _ ->
        err $ "Application to a non-function type " <> tshow fnty

  Fun args body -> do
    argBindings <- traverse (\id -> (,) id <$> freshType) args
    bodyty <- infer (foldr (\(k, v) -> Map.insert k v) ctx argBindings) body
    pure (TyFun (map snd argBindings) bodyty)
