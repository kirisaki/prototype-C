module Types where

data Expr'
  = Apply Expr Expr
  | Lambda String Expr
  | Var String
  | Lit Lit
  deriving (Eq)

newtype Lit = LitInt Int deriving (Show, Eq)

instance Show Expr' where
  show (Apply x y) = "(Apply" <> show x <> show y <> ")"
  show (Lambda x expr) = "(Lambda \"" <> x <> "\" " <> show expr <> ")"
  show (Var x) = "(Var \"" <> x <> "\")"
  show (Lit v) = "(Lit " <> show v <> ")"

data Located a = At Position a deriving (Eq, Show)

data Position
  = SrcPos Int Int
  | FromBuiltin
  | Dummy
  deriving (Eq, Show)

type Expr = Located Expr'

data Type
  = TyVar String
  | TyFun Type Type
  deriving (Show, Eq, Ord)

data Value
  = VInt Int
  | VVar String
  | VApp Value Value
  | VLam (Value -> Value)

instance Show Value where
  show (VInt x) = show x
  show (VVar x) = "<Var " <> x <> ">"
  show VApp{} = "<Apply>"
  show VLam{} = "<Lambda>"

withDummy :: a -> Located a
withDummy = At Dummy
