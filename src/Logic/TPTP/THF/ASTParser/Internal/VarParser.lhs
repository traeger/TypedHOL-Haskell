\section{Parser for Terms}

\subsection{Usage}
\begin{code}
module Logic.TPTP.THF.ASTParser.Internal.VarParser where

import Logic.TPTP.ParserCore
import Logic.TPTP.THF.AST
import Logic.TPTP.THF.ASTParser.Internal.TypeParser

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
varParser :: Parser (THFVar ())
varParser = do
  name <- varIdentifier
  symbol ":"
  typeFound <- typeParser
  
  return $ THFVar () typeFound name

varsParser :: Parser [THFVar ()]
varsParser = listElements varParser
\end{code}

