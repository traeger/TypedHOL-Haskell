\section{THF Parser}

\begin{code}
module Logic.TPTP.THF.AST 
( parseTest, parseJust, parseFile
, typeExpr
, constExpr
, varExpr, varsExpr
, termExpr, termExpr', termExprRaw, termExprTest

, abstractTHFFormulae
, abstractTHFFile
) where

import Logic.TPTP.ParserCore
import Logic.TPTP.THF.AST.Internal
import Logic.TPTP.THF.AST.Internal.TypeParser
import Logic.TPTP.THF.AST.Internal.ConstParser
import Logic.TPTP.THF.AST.Internal.VarParser
import Logic.TPTP.THF.AST.Internal.TermParser

import Logic.TPTP.THF.AST.Internal.AbstractTHFFormulaeParser
import Logic.TPTP.THF.AST.Internal.AbstractTHFFileParser

import Data.Map as Map

termExprRaw :: Parser HOLTerm
termExprRaw = termExpr undefined

termExprTest :: Parser HOLTerm
termExprTest = termExpr consts

a = HOLConst (HOLBaseType "o") "a"
h = HOLConst (HOLFuncType (HOLBaseType "o") (HOLBaseType "o")) "h"

consts
  = insertConst a
  $ insertConst h
  $ Map.empty
\end{code}
