{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
module Lib
    ( someFunc
    ) where

import Control.Exception.Safe
import Control.Applicative
import Control.Monad
import Control.Monad.State
import qualified Data.Map.Strict as MA
import qualified Data.List as L
import qualified Data.Set as SE
import Types
import Lexer
import Parser
import Debug.Trace


type Eval a = forall m. MonadThrow m => StateT (MA.Map String Value) m a

compile :: MonadThrow m => Expr -> m Value
compile (At pos expr) = go expr
  where
    go = \case
      Var x -> pure $ VVar x
      Lit l -> lit l
      Lambda k x -> abstract k <$> compile x
      Apply f x -> liftA2 VApp (compile f) (compile x)

    lit (LitInt x) = pure $ VInt x

abstract :: String -> Value -> Value
abstract x (VApp f y) = combS (abstract x f) (abstract x y)
abstract x (VVar k) | x == k = combI
abstract _ k = combK k

combI :: Value
combI = VLam id

combK :: Value -> Value
combK = VApp (VLam $ \x -> VLam $ const x)

combS :: Value -> Value -> Value
combS f = VApp (VApp c f)
  where
    c = VLam $ \f -> VLam $ \g -> VLam $ \x -> f ! x ! (g ! x)

infixl 0 !
(!) :: Value -> Value -> Value
VLam f ! x = f x

link :: Value -> Eval Value
link (VApp fun arg) = liftA2 (!) (link fun) (link arg)
link (VVar name) = MA.lookup name <$> get >>= \case
  Just v -> link v
  _ -> throwString "not declared"
link e = pure e

evalExpr :: Expr -> Eval Value
evalExpr expr = compile expr >>= link


type Context = MA.Map String Type

type Constraint = SE.Set (Type, Type)

type Assign = (String, Type)


constraintType :: MonadThrow m => Context -> Expr -> [String] -> m (Type, Constraint, [String])
constraintType _ (At _ (Lit LitInt {})) names = pure (TyVar "Int", SE.empty, names)
constraintType ctx (At _ (Var x)) names = do
  typ <- case MA.lookup x ctx of
    Just typ' -> pure typ'
    Nothing -> throwString $ "not found type variable: " <> show x
  pure (typ, SE.empty, names)
constraintType ctx (At _ (Lambda x t1)) names = do
  (xt, names0) <- newTyVar names
  let ctx0 = MA.insert x xt ctx
  (typ1, cst1, names1) <- constraintType ctx0 t1 names0
  pure (TyFun xt typ1, cst1, names1)
constraintType ctx (At _ (Apply t1 t2)) names = do
  (typ1, cst1, names1) <- constraintType ctx t1 names
  (typ2, cst2, names2) <- constraintType ctx t2 names1
  (typ, names3) <- newTyVar names2
  let cst = SE.unions [cst1, cst2, SE.singleton (typ1, TyFun typ2 typ)]
  pure (typ, cst, names3)
constraintType ctx (At _ (Let [] expr)) names = constraintType ctx expr names
constraintType ctx (At _ (Let (Decl k expr:ds) expr')) names = do
  (typ1, _, _) <- constraintType ctx expr names
  let ctx' = MA.insert k typ1 ctx
  constraintType ctx' (withDummy (Let ds expr')) names


newTyVar :: MonadThrow m => [String] -> m (Type, [String])
newTyVar (n:ns) = pure (TyVar n, ns)
newTyVar []   = throwString "no unused type variables"

unify :: MonadThrow m => Constraint -> m [Assign]
unify c
  | SE.null c = pure []
  | otherwise =
    let
      eq = SE.elemAt 0 c
      c' = SE.deleteAt 0 c
    in
      case eq of
        (s, t) | s == t -> unify c'
        (TyVar x, t) | SE.notMember x (freeVars t) -> do
                         let a = (x, t)
                         as <- unify (assignConstraint [a] c')
                         pure (a:as)
        (s, TyVar x) | SE.notMember x (freeVars s) -> do
                                let a = (x, s)
                                as <- unify (assignConstraint [a] c')
                                pure (a:as)
        (TyFun s1 s2, TyFun t1 t2) ->
          unify (SE.insert (s1, t1) (SE.insert (s2, t2) c'))
        _ -> throwString "invalid constraints"

freeVars :: Type -> SE.Set String
freeVars (TyVar x) = SE.singleton x
freeVars (TyFun t1 t2) = SE.union (freeVars t1) (freeVars t2)

assignType :: [Assign] -> Type -> Type
assignType as t =
  L.foldl' (flip assignType') t as

assignType' :: Assign -> Type -> Type
assignType' (y, s) t'@(TyVar x)
  | x == y = s
  | otherwise = t'
assignType' a (TyFun t1 t2) = TyFun (assignType' a t1) (assignType' a t2)

assignConstraint :: [Assign] -> Constraint -> Constraint
assignConstraint as cst =
  L.foldl' go cst as
  where
    go cst' a = SE.map (\(t1, t2) -> (assignType' a t1, assignType' a t2)) cst'

someFunc :: IO ()
someFunc = do
  let s0 = "g = f; main = (\\x -> \\y -> f x y)"
  let p = alexSetUserState (AlexUserState MA.empty) >> parser
  let Right t0 = runAlex s0 p
  print t0

  let names = show <$> [1 :: Int ..]
  (ty0, c0, _) <- constraintType
                  (MA.fromList
                    [ ("f", TyFun (TyVar "Int") (TyFun (TyVar "Int") (TyVar "Int")))
                    ]) t0 names
  print c0
  a0 <- unify c0
  print a0
  print $ assignType a0 ty0
