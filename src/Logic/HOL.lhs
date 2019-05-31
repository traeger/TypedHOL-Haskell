\begin{code}
module Logic.HOL (
  I.HOLTyped(..),
  I.ToHOL(..), I.FromHOL(..),
  I.HOLVar(..),
  I.HOLConst(..),
  I.HOLTerm(..),
  I.var, I.constant,
  I.not, I.forall, I.exists, I.app, I.lam, I.definition,
  (.->), (.|), (.&), (.@),
  I.name, I.def,
  S.SomeHOLConst(..), S.SomeHOLVar(..), S.SomeHOLTerm(..), S.gen, S.unGen,
  pretty, prettyTyped,
) where

import Logic.HOL.Internal as I
import Logic.HOL.Some as S
import Data.Text.Prettyprint.Doc
\end{code}