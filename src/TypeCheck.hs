{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module TypeCheck where

import CustomPrelude
import Control.Monad
import qualified Data.Map as Map

import Syntax

type Context = Map Ident Type

lookup :: Ident -> Context -> TC Type
lookup id ctx = maybe (Left ("undefined : " <> tshow id)) pure $ Map.lookup id ctx

type TC = Either Text

err = Left

tcModule :: [Declaration] -> TC [Declaration]
tcModule = fmap (reverse . fst) . foldM
  (\(acc, ctx) decl -> do
    (ident, ty, decl) <- case decl of
      Def{ident,body} -> do
        ty <- infer ctx body
        pure (ident, ty, decl{type_ = Just ty})
      Declare{ident,declareType} ->
        pure (ident, declareType, decl)
    pure (decl : acc, Map.insert ident ty ctx)
  )
  ([], mempty)

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
        when (argtys /= argtysInferred) $
          err $ "Argument type mismatch in application: " <> tshow argtys <> " vs " <> tshow argtysInferred
        pure retty
      _ ->
        err $ "Application to a non-function type " <> tshow fnty

  Fun args body -> do
    bodyty <- infer (foldr (\(k, v) -> Map.insert k v) ctx args) body
    pure (TyFun (map snd args) bodyty)
