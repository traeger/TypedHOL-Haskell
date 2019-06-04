\section{Abstract Parser for THF Files}
Parse a thf file without parsing the actual definition part of thf formulaes.

\subsection{Usage}
\begin{terminal}
\end{terminal}

\begin{code}
module Logic.TPTP.THF.AST.Internal.AbstractTHFFileParser where

import Logic.TPTP.ParserCore
import Logic.TPTP.THF.AST.Internal
import Logic.TPTP.THF.AST.Internal.AbstractTHFFormulaeParser

import Control.Monad
import Text.Megaparsec
import Control.Monad.Combinators

\end{code}

\begin{code}
abstractTHFFile :: Parser AbstractTHFFile
abstractTHFFile = AbstractTHFFile <$> some abstractTHFFormulae
\end{code}
