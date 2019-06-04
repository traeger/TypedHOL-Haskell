\section{Parser for Constant Declerations}

\subsection{Usage}
\begin{terminal}
*> parseJust constExpr "h: \$o"
*> parseJust constExpr "h: \$i"
*> parseJust constExpr "h: \$o < \$o"
*> parseJust constExpr "h: \$i < \$o"
*> parseJust constExpr "h: \$o < \$i"
\end{terminal}

\begin{code}
module Logic.TPTP.THF.AST.Internal.ConstParser where

import Logic.TPTP.ParserCore
import Logic.TPTP.THF.AST.Internal
import Logic.TPTP.THF.AST.Internal.TypeParser

import Control.Monad
import Text.Megaparsec
import Control.Monad.Combinators.Expr

\end{code}
Parse a THF constant declaration. Examples are:
\begin{itemize}
  \item $x: \$i$
  \item $b: \$o$
  \item $f: \$b < \$b$
  \item $g: \$b < \$i < \$b$
\end{itemize}

\begin{code}
constExpr :: Parser HOLConst
constExpr = do
  name <- constIdentifier
  symbol ":"
  typeFound <- typeExpr
  
  return $ HOLConst typeFound name
\end{code}
