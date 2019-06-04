\begin{code}
module Logic.HOL 
( I.HOLTyped(..)
, I.ToHOL(..), I.FromHOL(..)
, I.HOLVar(..)
, I.HOLConst(..)
, I.HOLTerm(..)
, I.var, I.constant
, I.not, I.forall, I.exists, I.app, I.lam, I.definition
, (.->), (.|), (.&), (.@), (.=)
, I.name, I.def
, S.SomeHOLConst(..), S.SomeHOLVar(..), S.SomeHOLTerm(..)
, S.gen, S.unGen, S.convert0, S.convert1, S.convert2, S.convert2f
, pretty, prettyTyped
) where

import Logic.HOL.Internal as I
import Logic.HOL.Internal.Some as S
import Data.Text.Prettyprint.Doc
\end{code}