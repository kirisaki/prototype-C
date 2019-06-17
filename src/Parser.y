
{
module Parser where
import Lexer
import Types
import qualified Data.Map.Strict as MA
import qualified Data.Set as SE
}

%name parser
%error { parseError }
%lexer { lexwrap } { TkEof }
%monad { Alex }
%tokentype { Token }

%token
' '     { TkWhite _ }
NUM     { TkNum ($$, _) }
VARSYM  { TkVarSym ($$, _) }
VARID   { TkVarId ($$, _) }
';'     { TkSep _ }
'='     { TkEqual _ }
'('     { TkLParen _ }
')'     { TkRParen _ }
'\\'    { TkLambda _ }
'->'    { TkArrow _ }

%left APPLY

%%
program :: { Expr }
program
  : decls { withDummy $ Let $1 (withDummy $ Var "main") }

decls :: { [Decl] }
decls
  : decl ';' decls { $1 : $3 }
  | decl { [$1] }

decl :: { Decl }
decl
  : VARID '=' expr { Decl $1 $3 }

expr :: { Expr }
expr
  : expr term %prec APPLY { withDummy $ Apply $1 $2 }
  | term { $1 }

term :: { Expr }
term
  : lambda                    { $1 }
  | varid                      { $1 }
  | num                       { $1 }
  | '(' expr ')'               { $2 }

varid :: { Expr }
varid
  : VARID { withDummy $ Var $1 }

num :: { Expr }
num
  : NUM { withDummy . Lit . LitInt $ fromIntegral $1 }


lambda :: { Expr }
lambda
  : '\\' VARID '->' expr { withDummy $ Lambda $2 $4 }


{


lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan >>=)

parseError :: Token -> Alex a
parseError t = alexError $ "parseError: " ++ show t


}
