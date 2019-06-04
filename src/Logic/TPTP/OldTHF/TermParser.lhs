\section{Parser for Terms}

\subsection{Usage}
\begin{terminal}
\end{terminal}

\begin{code}
{-# LANGUAGE ViewPatterns, RankNTypes, TypeFamilies #-}

module Logic.TPTP.THF.TermParser 
( termExpr
, termExprB
, termExprBB
) where

import Logic.HOL
import Data.Typeable
import qualified Type.Reflection as R

import Logic.TPTP.ParserCore
import Logic.TPTP.THF.TypeParser
import Logic.TPTP.THF.ConstParser

import Control.Monad
import Text.Megaparsec
import Control.Monad.Combinators.Expr

\end{code}
Parse a THF term. Examples are:
\begin{itemize}
\end{itemize}

\begin{code}
termExpr :: (Typeable u, Typeable t) => Parser (HOLTerm u t)
termExpr = makeExprParser termTerm termOperators

termTerm :: (Typeable u, Typeable t) => Parser (HOLTerm u t)
termTerm = parens termTerm
  <|> (convert0 T) <$ rword "$true"
  <|> (convert0 F) <$ rword "$false" 

termOperators :: (Typeable u, Typeable t) => [[Operator Parser (HOLTerm u t)]]
termOperators =
  [ [ InfixL ((convert2 (.->)) <$ symbol "&") ]
  , [ InfixL ((convert2f (.@)) <$ symbol "@") ]
  ]

termExprB :: Parser (HOLTerm () Bool)
termExprB = termExpr
termExprBB :: Parser (HOLTerm () (Bool -> Bool))
termExprBB = termExpr

\end{code}

