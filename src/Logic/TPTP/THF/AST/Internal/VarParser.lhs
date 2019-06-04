\section{Parser for Terms}

\subsection{Usage}
\begin{code}
module Logic.TPTP.THF.AST.Internal.VarParser where

import Logic.TPTP.ParserCore
import Logic.TPTP.THF.AST.Internal
import Logic.TPTP.THF.AST.Internal.TypeParser

import Control.Monad
import Text.Megaparsec
import Control.Monad.Combinators.Expr

import Data.Map (Map)
import Data.Map as Map

\end{code}

Parse a THF var. Examples are:
\begin{itemize}
  \item $X: \$i$
  \item $B: \$o$
  \item $F: \$b < \$b$
  \item $G: \$b < \$i < \$b$
\end{itemize}
\begin{code}
varExpr :: Parser HOLVar
varExpr = do
  name <- varIdentifier
  symbol ":"
  typeFound <- typeExpr
  
  return $ HOLVar typeFound name

varsExpr :: Parser [HOLVar]
varsExpr = listElements varExpr
\end{code}

