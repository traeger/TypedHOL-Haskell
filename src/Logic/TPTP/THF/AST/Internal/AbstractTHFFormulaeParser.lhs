\section{Abstract Parser for THF Formulae}
Parse a thf formulae without parsing the actual definition part.

\subsection{Usage}
\begin{terminal}
\end{terminal}

\begin{code}
module Logic.TPTP.THF.AST.Internal.AbstractTHFFormulaeParser where

import Logic.TPTP.ParserCore
import Logic.TPTP.THF.AST.Internal

import Control.Monad
import Text.Megaparsec
import Control.Monad.Combinators

\end{code}

\begin{code}
abstractTHFFormulae :: Parser AbstractTHFFormulae
abstractTHFFormulae = do
    symbol "thf("
    name <- identifier
    symbol ","
    formulaeType <- identifier
    symbol ","
    definition <- manyTill anySingle (symbol ").")
    case formulaeType of
      "type" ->       return $ AbstractTHFType name definition
      "definition" -> return $ AbstractTHFDefinition name definition
      "conjecture" -> return $ AbstractTHFConjecture name definition
\end{code}
