{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

module TypeCheck where

import CustomPrelude
import Control.Monad
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Bifunctor
import Control.Monad.State

import Syntax

type Context = Map Ident Type

lookup :: Ident -> Context -> TC Type
lookup id ctx = maybe (err ("undefined : " <> tshow id)) pure $ Map.lookup id ctx

type Substitution = Map Ident Type

data S = S
  { substitution :: Substitution
  , nextUnique :: Int
  }

initialState :: S
initialState = S { substitution = mempty, nextUnique = 0 }

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
        (body, ty) <- infer ctx body
        pure (ident, ty, decl{type_ = Just ty, body = body})
      Declare{ident,declareType} ->
        pure (ident, declareType, decl)
    pure (decl : acc, Map.insert ident ty ctx)
  )
  ([], mempty)

infer :: Context -> Expr -> TC (Expr, Type)
infer ctx = \case

  expr@(Var id) ->
    (expr,) <$> lookup id ctx

  expr@(Lit (IntLiteral _)) ->
    pure (expr, TyVar (Ident "Int"))

  expr@(Lit (StringLiteral _)) ->
    pure (expr, TyVar (Ident "String"))

  expr@(Lit (ArrayLiteral exprs)) -> do
    exprsTyped <- traverse (infer ctx) exprs
    let tys = map snd exprsTyped
    case tys of
      (ty:_) -> do
        when (any (/= ty) tys) $
          err $ "Element type mismatch in array, expected " <> ppType ty <> "; " <> tshow tys

        pure (Lit $ ArrayLiteral $ fst <$> exprsTyped, TyApp (TyVar (Ident "Array")) [ty])

      [] -> -- (forall [a] (Array a))
        err "Can't yet type an empty array :("

  -- (f a b c)
  -- (forall [a b c] t) -> [a b c], t
  App fn args -> do
    (fn, (tvs, fnty)) <- second stripForall <$> infer ctx fn
    let tvSet = Set.fromList tvs
    argsActual <- traverse (infer ctx) args
    case fnty of
      TyFun argtys retty -> do
        when (length argtys /= length argsActual) $
          err $ "Argument type mismatch in application: " <> tshow argtys <> " vs " <> tshow argsActual

        (arginfos, sub) <- extractSubst $
          forM (zip argsActual argtys) $ \((arg, actualType), expectedType) ->
            unifyD tvSet (stripDynamic actualType) (stripDynamic expectedType) arg

        let modifyCtx = foldr (\(modifyCtx, _, _) acc -> modifyCtx . acc) id arginfos
        let shouldDynamicize = any (\(_, d, _) -> d == Dynamicize) arginfos
        let argExprs = map (\(_, _, expr) -> expr) arginfos
        let dynamicizeIfNeeded appExpr resultTy =
              if shouldDynamicize then
                case stripDynamic resultTy of
                  (Dynamic, ty) ->
                    (appExpr, ty)
                  (Static, ty) ->
                    (App (Var (Ident "dynamic/pure")) [appExpr], TyApp (TyVar (Ident "Dynamic")) [ty])
              else
                (appExpr, resultTy)

        pure $ first modifyCtx $ dynamicizeIfNeeded (App fn argExprs) (subst sub retty)
      _ ->
        err $ "Application to a non-function type " <> tshow fnty

  Fun args body -> do
    (body, bodyty) <- infer (foldr (\(k, v) -> Map.insert k v) ctx args) body
    pure (Fun args body, TyFun (map snd args) bodyty)

  expr@(Block exprs) -> do
    exprsTyped <- traverse (infer ctx) exprs
    pure (Block (map fst exprsTyped), if null exprsTyped then TyVar (Ident "Unit") else snd (last exprsTyped))

  Let binders body -> do
    (binders, body, ty) <- tcLet ctx binders body
    pure (Let binders body, ty)

  expr ->
    terror $ "TypeCheck: unhandled: " <> ppExpr expr

tcLet :: Context -> [(Ident, Expr)] -> Expr -> TC ([(Ident, Expr)], Expr, Type)
tcLet ctx [] body = do
  (body, ty) <- infer ctx body
  pure ([], body, ty)
tcLet ctx ((ident, expr):binders) body = do
  (expr, ty) <- infer ctx expr
  (binders, body, resultTy) <- tcLet (Map.insert ident ty ctx) binders body
  pure ((ident, expr):binders, body, resultTy)

stripForall :: Type -> ([Ident], Type)
stripForall = \case
  TyForall tvs ty -> (tvs, ty)
  ty -> ([], ty)

data Dynamicity = Dynamic | Static deriving (Eq, Show)

stripDynamic :: Type -> (Dynamicity, Type)
stripDynamic = \case
  TyApp (TyVar (Ident "Dynamic")) [ty] -> (Dynamic, ty)
  ty -> (Static, ty)

type Unknowns = Set Ident

solve :: Ident -> Type -> TC ()
solve unknown ty = do
  -- TODO: occurs check
  modify (\s -> s { substitution = Map.insert unknown ty (substitution s) })

extractSubst :: TC a -> TC (a, Substitution)
extractSubst block = do
  prev <- get
  ret <- block
  sub <- gets substitution
  modify (\s -> s { substitution = substitution prev })
  pure (ret, sub)

generateName :: Text -> TC Ident
generateName prefix = do
  id <- gets nextUnique
  modify (\s -> s { nextUnique = id + 1 })
  pure $ Ident $ prefix <> "_$" <> tshow id

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

data Dynamicize = Dynamicize | NoDynamicize deriving (Eq, Show)

unifyD :: Unknowns -> (Dynamicity, Type) -> (Dynamicity, Type) -> Expr -> TC (Expr -> Expr, Dynamicize, Expr)
unifyD u (Dynamic, ty1) (Dynamic, ty2) arg = do
  unify u ty1 ty2
  pure (id, NoDynamicize, arg)
unifyD u (Static, ty1) (Static, ty2) arg = do
  unify u ty1 ty2
  pure (id, NoDynamicize, arg)

-- (int->string (dynamic/pure 1)) -> (dynamic/bind (dynamic/pure 1) (fn [$1] (dynamic/pure (int->string $1))))
unifyD u (Dynamic, ty1) (Static, ty2) arg = do
  unify u ty1 ty2
  sub <- gets substitution
  nm <- generateName ""
  pure 
    ( \ctx -> App (Var (Ident "dynamic/bind")) [arg, Fun [(nm, subst sub ty2)] ctx]
    , Dynamicize
    , Var nm )

-- (dynamic/subscribe 1 ...)  ->  (dynamic/subscribe (dynamic/pure 1) ...)
unifyD u (Static, ty1) (Dynamic, ty2) arg = do
  unify u ty1 ty2
  pure (id, NoDynamicize, App (Var (Ident "dynamic/pure")) [arg])

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

f :: IO ()
f = do
  x <- getLine
  case x of
    "" -> foo
    _ -> bar

f =
  case "fo" <> !getLine of
    "" -> foo
    _ -> bar
