\section{THF Parser}

\begin{code}
module Logic.TPTP.THF.Parser 
( parseTest
, parseJust
, typeExpr
, typeDeclerationExpr
) where

import Logic.TPTP.ParserCore
import Logic.TPTP.THF.TypeParser
import Logic.TPTP.THF.TypeDeclerationParser
\end{code}

\subsection{Parse types}
*Logic.TPTP.THF.Parser> parseJust typeExpr "$o"
Bool
*Logic.TPTP.THF.Parser> parseJust typeExpr "$o < $o"
Bool -> Bool

\subsection{Parse type declarations}
*Logic.TPTP.THF.Parser> parseJust typeDeclerationExpr "h: $o" :: HOLConst (Bool) ()
h :: Bool

*Logic.TPTP.THF.Parser> parseJust typeDeclerationExpr "h: $o < $o" :: HOLConst (Bool -> Bool) ()
h :: Bool -> Bool

*Logic.TPTP.THF.Parser> parseJust typeDeclerationExpr "h: $o" :: HOLConst (Bool -> Bool) ()
*** Exception: 1:6:
  |
1 | h: $o
  |      ^
"expected hol type: Bool -> Bool, found: Bool"
