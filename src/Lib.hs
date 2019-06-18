{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Lib
    ( someFunc
    ) where

import Control.Exception.Safe
import Control.Applicative
import Control.Monad
import Control.Monad.RWS
import qualified Data.Map.Strict as MA
import qualified Data.List as L
import qualified Data.Set as SE
import Types
import Lexer
import Parser
import Debug.Trace

type Env = MA.Map String Scheme

type Subst = MA.Map String Type

type Unifier = (Subst, [Constraint])

type Infer a = forall m. MonadThrow m => RWST Env () Int m a


type Context = MA.Map String Type

type Constraint = (Type, Type)

type Assign = (String, Type)

class Substitutable a where
  apply :: Subst -> a -> a
  ftv   :: a -> SE.Set String
  
instance Substitutable Type where
  apply _ (TyCon a)       = TyCon a
  apply s t@(TyVar a) = MA.findWithDefault t a s
  apply s (TyFun t1 t2) = TyFun (apply s t1) (apply s t2)

  ftv TyCon{}         = SE.empty
  ftv (TyVar a)       = SE.singleton a
  ftv (TyFun t1 t2) = SE.union (ftv t1) (ftv t2)

instance Substitutable Scheme where
  apply s (Forall as t)   = Forall as $ apply s' t
    where
      s' = foldr MA.delete s as
  ftv (Forall as t) = SE.difference (ftv t) (SE.fromList as)

instance Substitutable Constraint where
   apply s (t1, t2) = (apply s t1, apply s t2)
   ftv (t1, t2) = SE.union (ftv t1) (ftv t2)

instance Substitutable a => Substitutable [a] where
  apply = fmap . apply
  ftv   = foldr (SE.union . ftv) SE.empty

instance Substitutable Env where
  apply s env = MA.map (apply s) env
  ftv env = ftv $ MA.elems env
  
lookupEnv :: String -> Infer Type
lookupEnv x = do
  env <- ask
  case MA.lookup x env of
    Nothing ->  throwString $ "unbound variable: " <> x
    Just s -> instantiate s

fresh :: Infer Type
fresh = do
    s <- get
    put $ s + 1
    pure . TyVar $ show s

  
instantiate ::  Scheme -> Infer Type
instantiate (Forall as t) = do
    as' <- mapM (const fresh) as
    let s = MA.fromList $ zip as as'
    pure $ apply s t

inEnv :: Env -> Infer a -> Infer a
inEnv env m = do
  let scope e = MA.union env (MA.difference e env)
  local scope m

infer :: Expr -> Infer (Type, [Constraint])
infer = \case
  At _ (Lit (LitInt _)) -> pure (tyInt, [])
  At _ (Lit (LitBool _)) -> pure (tyBool, [])

  At _ (Var x) -> do
    t <- lookupEnv x
    pure (t, [])

  At _ (Lambda x e) -> do
    tv <- fresh
    (t, c) <- inEnv (MA.singleton x (Forall [] tv)) (infer e)
    pure (TyFun tv t, c)

  At _ (Apply e1 e2) -> do
    (t1, c1) <- infer e1
    (t2, c2) <- infer e2
    tv <- fresh
    pure (tv, c1 <> c2 <> [(t1, TyFun t2 tv)])

  At _ (Let ds e') -> do
    env <- ask
    (xs, ts, subs, cs) <- L.unzip4 <$>
      (forM ds $ \(Decl x e) -> do
          (t, c) <- infer e
          sub <- solve (MA.empty, c)
          pure (x, generalize (apply sub env) (apply sub t), sub, c)
      )
    (t2, c2) <- inEnv (MA.fromList (L.zip xs ts)) $ local (apply $ MA.unions subs) (infer e')
    pure (t2, L.concat cs <> c2)

generalize :: Env -> Type -> Scheme
generalize env t  = Forall as t
    where
      as = SE.toList $ SE.difference (ftv t)  (ftv env)

solve :: MonadThrow m => Unifier -> m Subst
solve (su, []) = pure su
solve (su, (t1, t2):cs) = do
  su1 <- unifies t1 t2
  solve (MA.union su1 su, apply su1 cs)

unifyMany :: MonadThrow m => [Type] -> [Type] -> m Subst
unifyMany [] [] = pure MA.empty
unifyMany (t1 : ts1) (t2 : ts2) = do
  su1 <- unifies t1 t2
  su2 <- unifyMany (fmap (apply su1) ts1) (fmap (apply su1) ts2)
  pure $ MA.union su1 su2
unifyMany t1 t2 = throwString $ "unification mismatch: " <> show t1 <> " " <> show t2

unifies :: MonadThrow m => Type -> Type -> m Subst
unifies t1 t2 | t1 == t2 = pure MA.empty
unifies (TyVar v) t = bind v t
unifies t (TyVar v) = bind v t
unifies (TyFun t1 t2) (TyFun t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies t1 t2 = throwString $ "failed unification: " <> show t1 <> " " <> show t2
  
bind :: MonadThrow m => String -> Type -> m Subst
bind a t | t == TyVar a = pure MA.empty
         | occurs a t = throwString $ "occurs check failed: " <> show a <> " " <> show t
         | otherwise = pure $ MA.singleton a t

occurs :: String -> Type -> Bool
occurs a t = SE.member a (ftv t)

someFunc :: IO ()
someFunc = do
  let s0 = "main = let g = f; h = g in \\x -> \\y -> h x y"
  let p = alexSetUserState (AlexUserState MA.empty) >> parser
  let Right t0 = runAlex s0 p
  print t0
  print =<< runRWST
    (infer t0)
    (MA.fromList [("f", Forall [] $ TyFun tyInt (TyFun tyInt tyInt))
                 ]) 0
