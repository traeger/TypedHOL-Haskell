\section{THF Parser}

\begin{code}
module Logic.TPTP.THF.ASTParser 
( parseTest, parseJust, parseFile
, typeParser
, constParser
, varParser, varsParser
, termParser, termParser', termParserRaw, termParserTest

, abstractTHFParser
, abstractTHFFileParser
) where

import Logic.TPTP.ParserCore
import Logic.TPTP.THF.AST

import Logic.TPTP.THF.ASTParser.Internal.TypeParser
import Logic.TPTP.THF.ASTParser.Internal.ConstParser
import Logic.TPTP.THF.ASTParser.Internal.VarParser
import Logic.TPTP.THF.ASTParser.Internal.TermParser

import Logic.TPTP.THF.ASTParser.Internal.AbstractTHFFormulaeParser
import Logic.TPTP.THF.ASTParser.Internal.AbstractTHFFileParser

import Data.Map as Map

termParserRaw :: Parser HOLTerm
termParserRaw = termParser undefined

termParserTest :: Parser HOLTerm
termParserTest = termParser consts

a = HOLConst (HOLBaseType "o") "a"
h = HOLConst (HOLFuncType (HOLBaseType "o") (HOLBaseType "o")) "h"

consts
  = insertConst a
  $ insertConst h
  $ Map.empty
\end{code}
