module Types where


data Expr'
  = Apply Expr Expr
  | Lambda String Expr
  | Var String
  | Lit Lit
  | Let [Decl] Expr
  | If Expr Expr Expr
  deriving (Eq)

data Lit
  = LitInt Int
  | LitBool Bool
  deriving (Show, Eq)

instance Show Expr' where
  show (Apply x y) = "(Apply" <> show x <> show y <> ")"
  show (Lambda x expr) = "(Lambda \"" <> x <> "\" " <> show expr <> ")"
  show (Var x) = "(Var \"" <> x <> "\")"
  show (Lit v) = "(Lit " <> show v <> ")"
  show (If c t f) = "(If " <> show c <> " then " <> show t <> " else " <> show f <> ")"
  show (Let ds e) = show ds <> show e

data Located a = At Position a deriving (Eq, Show)

data Position
  = SrcPos Int Int
  | FromBuiltin
  | Dummy
  deriving (Eq, Show)

type Expr = Located Expr'

data Type
  = TyVar String
  | TyCon String
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

data Decl
  = Decl String Expr
  deriving (Eq, Show)

data Scheme = Forall [String] Type deriving(Show, Eq,Ord)

tyInt, tyBool :: Type
tyInt = TyCon "Int"
tyBool = TyCon "Bool"

