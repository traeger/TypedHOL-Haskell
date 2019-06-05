\section{Parser for Types}

\subsection{Usage}
\begin{terminal}
*Logic.TPTP.THF.Parser> parseJust typeParser "\$o"
*Logic.TPTP.THF.Parser> parseJust typeParser "\$o > \$o"
*Logic.TPTP.THF.Parser> parseJust typeParser "\$o > \$o > \$o"
*Logic.TPTP.THF.Parser> parseJust typeParser "(\$o > \$o) > \$o"
*Logic.TPTP.THF.Parser> parseJust typeParser "\$o > (\$o > \$o)"
\end{terminal}

\begin{code}
module Logic.TPTP.THF.ASTParser.Internal.TypeParser where

import Logic.HOL

import Logic.TPTP.ParserCore
import Logic.TPTP.THF.AST

import Text.Megaparsec
import Control.Monad.Combinators.Expr

\end{code}
Parse a THF type. Examples are:
\begin{itemize}
  \item $\$i$
  \item $\$o$
  \item $\$b > \$b$
  \item $\$b > \$i > \$b$
\end{itemize}
\begin{code}
typeParser :: Parser THFType 
typeParser = makeExprParser typeTerm typeOperators

typeTerm :: Parser THFType
typeTerm = parens typeParser
  <|> (THFBaseType "o") <$ rword "$o"
  <|> (THFBaseType "i") <$ rword "$i" 
  -- we use int for now, since the $i type is not used in any specific why 

typeOperators :: [[Operator Parser THFType]]
typeOperators =
  [ [ InfixL $ THFFuncType <$ symbol ">" ]
  ]
\end{code}
