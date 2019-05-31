\section{Parser for Types}

\subsection{Usage}
*Logic.TPTP.THF.Parser> parseJust typeExpr "$o"
Bool
*Logic.TPTP.THF.Parser> parseJust typeExpr "$o < $o"
Bool -> Bool

\begin{code}
module Logic.TPTP.THF.TypeParser 
( typeExpr
, holTypeError
) where

import Logic.HOL
import Data.Typeable

import Logic.TPTP.ParserCore

import Text.Megaparsec
import Control.Monad.Combinators.Expr

holTypeError :: TypeRep -> TypeRep -> ParserT s a
holTypeError expected found = textError $ "expected hol type: " ++ (show expected) ++ ", found: " ++ (show found)

\end{code}
Parse a THF type. Examples are:
\begin{itemize}
  \item $\$i$
  \item $\$o$
  \item $\$b < \$b$
  \item $\$b < \$i < \$b$
\end{itemize}
\begin{code}
typeExpr :: Parser TypeRep
typeExpr = makeExprParser typeTerm typeOperators

typeTerm :: Parser TypeRep
typeTerm = parens typeExpr
  <|> (typeOf (undefined :: Bool)) <$ rword "$o"
  <|> (typeOf (undefined :: Int)) <$ rword "$i" 
  -- we use int for now, since the $i type is not used in any specific why 

typeOperators :: [[Operator Parser TypeRep]]
typeOperators =
  [ [ InfixL (mkFunTy <$ symbol "<") ]
  ]
\end{code}
