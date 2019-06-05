\section{Parser for THF Files}
Parse a thf file in 4 steps:
\begin{itemize}
\item step1: parse an abstract representation of file. Down to the thf(_name, _thf-type, _definition). structure
\item step2: parse all type definitions and generate a type lookup map.
\item step3: parse all axiom, definition and conjecture formulae using the type definitions
\end{itemize}

\subsection{Usage}
\begin{terminal}
\end{terminal}

\begin{code}
module Logic.TPTP.THF.ASTParser.Internal.THFFileParser where

import Logic.TPTP.ParserCore
import Logic.TPTP.THF.AST
import Logic.TPTP.THF.ASTParser.Internal.ConstParser
import Logic.TPTP.THF.ASTParser.Internal.AbstractTHFFileParser

import Control.Monad
import Text.Megaparsec
import Control.Monad.Combinators

import Data.Map (Map)

\end{code}

\begin{code}
step1 :: Parser AbstractTHFFile
step1 = abstractTHFFileParser

parseConst :: AbstractTHFType -> (THFConst ())
parseConst (AbstractTHFType name c) = parseJust constParser c

step2 :: AbstractTHFFile -> Map String (THFConst ())
step2 (AbstractTHFFile types formulae) = mkConstMap $ map parseConst types

--step3 :: AbstractTHFFile -> Map String (THFConst ()) -> []
\end{code}
