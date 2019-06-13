{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE RankNTypes #-}
module Lib
    ( someFunc
    ) where

import Control.Exception.Safe
import Control.Applicative
import Control.Monad.State
import qualified Data.Map.Strict as MA
import Types

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

someFunc :: IO ()
someFunc = putStrLn "someFunc"
