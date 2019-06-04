\section{Abstract Parser for THF Formulae}
Parse a thf formulae without parsing the actual definition part.

\subsection{Usage}
\begin{terminal}
\end{terminal}

\begin{code}
module Logic.TPTP.THF.ASTParser.Internal.AbstractTHFFormulaeParser where

import Logic.TPTP.ParserCore
import Logic.TPTP.THF.AST

import Control.Monad
import Text.Megaparsec
import Control.Monad.Combinators

\end{code}

\begin{code}
abstractTHFParser :: Parser (Either AbstractTHFType AbstractTHFFormulae)
abstractTHFParser = do
    symbol "thf("
    name <- identifier
    symbol ","
    formulaeType <- identifier
    symbol ","
    definition <- normalizeTPTPSpaces <$> manyTill anySingle (symbol ").")
    case formulaeType of
      "type" ->       return $ Left $ AbstractTHFType name definition
      "axiom" ->      return $ Right $ AbstractTHFAxiom name definition
      "definition" -> return $ Right $ AbstractTHFDefinition name definition
      "conjecture" -> return $ Right $ AbstractTHFConjecture name definition
\end{code}
