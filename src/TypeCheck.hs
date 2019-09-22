{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

module TypeCheck where

import CustomPrelude
import Control.Monad
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State

import Syntax

type Context = Map Ident Type

lookup :: Ident -> Context -> TC Type
lookup id ctx = maybe (err ("undefined : " <> tshow id)) pure $ Map.lookup id ctx

type Substitution = Map Ident Type

data S = S
  { substitution :: Substitution
  }

initialState :: S
initialState = S { substitution = mempty }

type TC = StateT S (Either Text)

run :: TC a -> Either Text a
run = flip evalStateT initialState

err :: Text -> TC a
err = lift . Left

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
    (tvs, fnty) <- stripForall <$> infer ctx fn
    let tvSet = Set.fromList tvs
    argtysInferred <- traverse (infer ctx) args
    case fnty of
      TyFun argtys retty -> do
--        when (argtys /= argtysInferred) $
--          err $ "Argument type mismatch in application: " <> tshow argtys <> " vs " <> tshow argtysInferred

        when (length argtys /= length argtysInferred) $
          err $ "Argument type mismatch in application: " <> tshow argtys <> " vs " <> tshow argtysInferred

        sub <- extractSubst $
          forM_ (zip argtys argtysInferred) $ \(t1, t2) ->
            unify tvSet t1 t2

        pure (subst sub retty)
      _ ->
        err $ "Application to a non-function type " <> tshow fnty

  Fun args body -> do
    bodyty <- infer (foldr (\(k, v) -> Map.insert k v) ctx args) body
    pure (TyFun (map snd args) bodyty)

stripForall :: Type -> ([Ident], Type)
stripForall = \case
  TyForall tvs ty -> (tvs, ty)
  ty -> ([], ty)

type Unknowns = Set Ident

solve :: Ident -> Type -> TC ()
solve unknown ty = do
  -- TODO: occurs check
  modify (\s -> s { substitution = Map.insert unknown ty (substitution s) })

extractSubst :: TC () -> TC Substitution
extractSubst block =
  withStateT (\s -> s { substitution = mempty }) $ do
    block
    gets substitution

subst :: Map Ident Type -> Type -> Type
subst sub = \case
  TyVar v ->
    case Map.lookup v sub of
      Just ty -> ty
      _       -> TyVar v

  TyApp f args ->
    TyApp (subst sub f) (subst sub <$> args)

  TyFun args ret ->
    TyFun (subst sub <$> args) (subst sub ret)

  TyForall ids ty ->
    let sub' = foldr Map.delete sub ids
          -- FIXME: this handles name shadowing,
          -- but NOT name capture! Consider:
          -- subst [(a, Maybe b)] (forall b. a -> b)
    in TyForall ids (subst sub' ty)

unify :: Unknowns -> Type -> Type -> TC ()
unify u t1 t2 = do
  sub <- gets substitution
  go (subst sub t1) (subst sub t2)

  where
  unifyError = err $ "Can't unify: " <> tshow t1 <> " and " <> tshow t2

  go (TyVar v) ty | v `Set.member` u = solve v ty
  go ty (TyVar v) | v `Set.member` u = solve v ty
  go (TyVar v1) (TyVar v2) | v1 == v2 = pure ()
  go (TyVar v) _ = unifyError

  go (TyApp f1 args1) (TyApp f2 args2) = do
    unify u f1 f2

    when (length args1 /= length args2) $
      err $ "Mismatched number of arguments in type application: " <> tshow args1 <> " vs " <> tshow args2

    forM_ (zip args1 args2) $ \(t1, t2) ->
      unify u t1 t2

  go (TyApp _ _) _ = unifyError

  go (TyFun args1 ret1) (TyFun args2 ret2) = do
    when (length args1 /= length args2) $
      err $ "Mismatched number of arguments when unifying function type: " <> tshow args1 <> " vs " <> tshow args2

    forM_ (zip args1 args2) $ \(t1, t2) ->
      unify u t1 t2

    unify u ret1 ret2

  go (TyFun _ _) _ = unifyError

  go (TyForall _ _) _ = err "Unifying foralls not yet supported"
