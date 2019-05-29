Experimental, not working at all.

\begin{code}
module Logic.TPTP.THF.TypeDeclerationParser 
( typeDeclerationExpr
, typeDeclerationExpr0
, HOLConst(..)
, HOLTerm(..)
) where

import Logic.HOL
import Data.Typeable

import Logic.TPTP.ParserCore
import Logic.TPTP.THF.TypeParser

import Control.Monad
import Text.Megaparsec
import Control.Monad.Combinators.Expr

\end{code}
Parse a THF type declaration. Examples are:
\begin{itemize}
  \item $x: \$i$
  \item $b: \$o$
  \item $f: \$b < \$b$
  \item $g: \$b < \$i < \$b$
\end{itemize}
\begin{code}
typeDeclerationExpr :: (Typeable t, Typeable u) => Parser (HOLConst t u)
typeDeclerationExpr = do
  name <- identifier
  symbol ":"
  typeFound <- typeExpr
  let c = constant name
  let typeExspected = getHOLType c

  when (typeFound /= typeExspected) $ holTypeError typeExspected typeFound

  return c

typeDeclerationExpr0 :: Parser (HOLConst Bool ())
typeDeclerationExpr0 = typeDeclerationExpr

\end{code}
