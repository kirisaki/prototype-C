{
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
  
module Lexer where

import Control.Monad.State
import qualified Data.Map.Strict as MA
import Debug.Trace
}

%wrapper "monadUserState"

$digit = 0-9
$smallalpha = a-z
$largealpha = A-Z
$alpha = [$smallalpha$largealpha]
$symbol = [\+\-\*\/]

tokens :-

<0>  $white+                       { skip }
<0>  $digit+                       { mkLx LxNum }
<0>  \;                            { mkLx LxSep }
<0>  \=                            { mkLx LxEqual }
<0>  \(                            { mkLx LxLParen }
<0>  \)                            { mkLx LxRParen }
<0>  \\                            { mkLx LxLambda }
<0>  \-\>                          { mkLx LxArrow }
<0>  [\=\+\-\*\/\^]                { mkLx LxVarSym }
<0>  let                            { mkLx LxLet }
<0>  in                            { mkLx LxIn }
<0>  $smallalpha$alpha*            { mkLx LxVarId }

{
data Lexeme
  = LxWhite
  | LxSep
  | LxEqual
  | LxLParen
  | LxRParen
  | LxVarSym
  | LxVarId
  | LxNum
  | LxLambda
  | LxArrow
  | LxLet
  | LxIn
  deriving (Eq, Show)

data Token
  = TkWhite AlexPosn
  | TkSep AlexPosn
  | TkEqual AlexPosn
  | TkLParen AlexPosn
  | TkRParen AlexPosn
  | TkVarSym ((String, Int, OpAssoc), AlexPosn)
  | TkVarId (String, AlexPosn)
  | TkNum (Integer, AlexPosn)
  | TkLambda AlexPosn
  | TkArrow AlexPosn
  | TkLet AlexPosn
  | TkIn AlexPosn
  | TkEof
  deriving (Eq, Show)

mkLx :: Lexeme -> AlexInput -> Int -> Alex Token
mkLx lx (pos, _, _, str) len =
  let
    t = take len str
  in
    case lx of
      LxWhite -> pure $ TkWhite pos
      LxSep -> pure $ TkSep pos
      LxEqual -> pure $ TkEqual pos
      LxLParen -> pure $ TkLParen pos
      LxRParen -> pure $ TkRParen pos
      LxVarId  -> pure $ TkVarId (t, pos)
      LxLambda  -> pure $ TkLambda pos
      LxArrow  -> pure $ TkArrow pos
      LxLet  -> pure $ TkLet pos
      LxIn  -> pure $ TkIn pos
      LxVarSym -> Alex $ (\s@AlexState{..} ->
                          case MA.lookup t (operators alex_ust) of
                            Just (n, asoc) -> Right (s, TkVarSym ((t, fromIntegral n, asoc), pos))
                            _ -> Left "unknown operator"
                         )
      LxNum -> pure $ TkNum (read t,  pos)

alexEOF :: Alex Token
alexEOF = pure TkEof

data OpAssoc
  = OpL
  | OpR
  deriving(Show,Eq)


data AlexUserState = AlexUserState { operators :: MA.Map String (Int, OpAssoc) }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState MA.empty

}
