\section{Parser for THF Files}
Parse a thf file in 4 steps:
\begin{itemize}
\item parse an abstract representation of file. Down to the thf(_name, _thf-type, _definition). structure
\item parse all type definitions and generate a type lookup map.
\item parse all axiom, definition and conjecture formulae using the type definitions
\end{itemize}

\subsection{Usage}
\begin{terminal}
\end{terminal}

\begin{code}
module Logic.TPTP.THF.ASTParser.Internal.AbstractTHFFileParser where

import Logic.TPTP.ParserCore
import Logic.TPTP.THF.AST
import Logic.TPTP.THF.ASTParser.Internal.AbstractTHFFormulaeParser

import Control.Monad
import Text.Megaparsec
import Control.Monad.Combinators

\end{code}

\begin{code}
step1 :: Parser AbstractTHFFile
step1 = abstractTHFFile

step2 :: AbstractTHFFile -> 
\end{code}
