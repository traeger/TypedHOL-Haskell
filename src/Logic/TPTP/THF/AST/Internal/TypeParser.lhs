\section{Parser for Types}

\subsection{Usage}
\begin{terminal}
*Logic.TPTP.THF.Parser> parseJust typeExpr "\$o"
*Logic.TPTP.THF.Parser> parseJust typeExpr "\$o < \$o"
*Logic.TPTP.THF.Parser> parseJust typeExpr "\$o < \$o < \$o"
*Logic.TPTP.THF.Parser> parseJust typeExpr "(\$o < \$o) < \$o"
*Logic.TPTP.THF.Parser> parseJust typeExpr "\$o < (\$o < \$o)"
\end{terminal}

\begin{code}
module Logic.TPTP.THF.AST.Internal.TypeParser where

import Logic.HOL

import Logic.TPTP.ParserCore
import Logic.TPTP.THF.AST.Internal

import Text.Megaparsec
import Control.Monad.Combinators.Expr

\end{code}
Parse a THF type. Examples are:
\begin{itemize}
  \item $\$i$
  \item $\$o$
  \item $\$b < \$b$
  \item $\$b < \$i < \$b$
\end{itemize}
\begin{code}
typeExpr :: Parser HOLType
typeExpr = makeExprParser typeTerm typeOperators

typeTerm :: Parser HOLType
typeTerm = parens typeExpr
  <|> (HOLBaseType "o") <$ rword "$o"
  <|> (HOLBaseType "i") <$ rword "$i" 
  -- we use int for now, since the $i type is not used in any specific why 

typeOperators :: [[Operator Parser HOLType]]
typeOperators =
  [ [ InfixL $ HOLFuncType <$ symbol "<" ]
  ]
\end{code}
