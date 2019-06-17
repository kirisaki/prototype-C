{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
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

type Unifier = (Subst, Constraint)

type Infer a = forall m. MonadThrow m => RWST Env () Int m a


type Context = MA.Map String Type

type Constraint = SE.Set (Type, Type)

type Assign = (String, Type)

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

applyType :: Subst -> Type -> Type
applyType _ a@(TyCon {}) = a
applyType s t@(TyVar a) = MA.findWithDefault t a s
applyType s (TyFun t1 t2) = TyFun (applyType s t1) (applyType s t2)
  
instantiate ::  Scheme -> Infer Type
instantiate (Forall as t) = do
    as' <- mapM (const fresh) as
    let s = MA.fromList $ zip as as'
    pure $ applyType s t

inEnv :: (String, Scheme) -> Infer a -> Infer a
inEnv (x, sc) m = do
  let scope e = MA.insert x sc (MA.delete x e)
  local scope m

infer :: Expr -> Infer (Type, Constraint)
infer = \case
  At _ (Lit (LitInt _)) -> pure (tyInt, SE.empty)
  At _ (Lit (LitBool _)) -> pure (tyBool, SE.empty)

  At _ (Var x) -> do
    t <- lookupEnv x
    pure (t, SE.empty)

  At _ (Lambda x e) -> do
    tv <- fresh
    (t, c) <- inEnv (x, Forall [] tv) (infer e)
    pure (TyFun tv t, c)

  At _ (Apply e1 e2) -> do
    (t1, c1) <- infer e1
    (t2, c2) <- infer e2
    tv <- fresh
    pure (tv, SE.unions [c1, c2, SE.singleton (t1, TyFun t2 tv)])

  At _ (Let [] e) -> infer e
  At _ (Let (Decl x e:ds) e') -> do
    env <- ask
    (t, c) <- infer e
    sub <-  solve (MA.empty, c)
    pure undefined

solve :: MonadThrow m => Unifier -> m Subst
solve (su, cs) =
  if SE.null cs
  then pure MA.empty
  else do
    let (cs', cs0) = SE.splitAt 1 cs
    let (t1, t2) = SE.elemAt 0 cs'
    --su1 <- unifies t1 t2
    undefined

someFunc :: IO ()
someFunc = do
  let s0 = "g = f; main = (\\x -> \\y -> f x y)"
  let p = alexSetUserState (AlexUserState MA.empty) >> parser
  let Right t0 = runAlex s0 p
  print t0
  print =<< runRWST (infer t0) MA.empty 0
