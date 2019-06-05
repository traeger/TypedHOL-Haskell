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

, step1, step2
) where

import Logic.TPTP.ParserCore
import Logic.TPTP.THF.AST

import Logic.TPTP.THF.ASTParser.Internal.TypeParser
import Logic.TPTP.THF.ASTParser.Internal.ConstParser
import Logic.TPTP.THF.ASTParser.Internal.VarParser
import Logic.TPTP.THF.ASTParser.Internal.TermParser

import Logic.TPTP.THF.ASTParser.Internal.AbstractTHFFormulaeParser
import Logic.TPTP.THF.ASTParser.Internal.AbstractTHFFileParser

import Logic.TPTP.THF.ASTParser.Internal.THFFileParser

import Data.Map as Map

termParserRaw :: Parser (THFTerm ())
termParserRaw = termParser undefined

termParserTest :: Parser (THFTerm ())
termParserTest = termParser consts

a = THFConst () (THFBaseType "o") "a"
h = THFConst () (THFFuncType (THFBaseType "o") (THFBaseType "o")) "h"

consts
  = insertConst a
  $ insertConst h
  $ Map.empty
\end{code}
