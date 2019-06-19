
{
module Parser where
import Lexer
import Types
import qualified Data.Map.Strict as MA
import qualified Data.Set as SE
import Debug.Trace
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
'if'    { TkIf _ }
'then'    { TkThen _ }
'else'    { TkElse _ }
'let'    { TkLet _ }
'in'    { TkIn _ }

%right IF
%left APPLY
%nonassoc DECL

%%
program :: { Expr }
program
  : decls { withDummy $ Let $1 (withDummy $ Var "main") }

decls :: { [Decl] }
decls
  : decl ';' decls %prec DECL { $1 <> $3 }
  | decl ';' { $1 }
  | decl { $1 }

decl :: { [Decl] }
decl
  : VARID '=' expr { [Decl $1 $3] }

expr :: { Expr }
expr
  : 'if' expr 'then' expr 'else' expr %prec IF { withDummy $ If $2 $4 $6 }
  | expr term %prec APPLY { withDummy $ Apply $1 $2 }
  | term { $1 }
  | lambda                    { $1 }
  | 'let' decls 'in' expr { withDummy $ Let $2 $4 }

term :: { Expr }
term
  : varid                      { $1 }
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
