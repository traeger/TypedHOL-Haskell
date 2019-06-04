\section{Abstract Parser for THF Files}
Parse a thf file without parsing the actual definition part of thf formulaes.

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

import Data.Either (partitionEithers)
\end{code}

\begin{code}
abstractTHFFileParser :: Parser AbstractTHFFile
abstractTHFFileParser = (uncurry AbstractTHFFile . partitionEithers) <$> some abstractTHFParser
\end{code}
