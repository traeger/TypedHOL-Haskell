
\begin{code}
{-# LANGUAGE GADTs, TypeFamilies #-}

module Logic.HOL.Some where

import Logic.HOL.Internal

import Data.Typeable
import Prelude hiding (forall, exists, not)
import Data.Text.Prettyprint.Doc

class Some s where
  type Something s
  gen :: (Typeable t) => (s t) -> Something s
  unGen :: (Typeable t) => Something s -> Maybe (s t)
\end{code}

Helper to generete List over multible types. Allows:
\begin{code}# example
x = constant "x" :: HOLConst () (Bool) 
h = constant "h" :: HOLConst () (Bool -> Bool) 
f = definition "f" \$ T .& x
l = [gen x, gen h, gen f]
\end{code}# example

\begin{code}
data SomeHOLConst u where
  SomeHOLConst :: (Typeable t) => !(HOLConst u t) -> SomeHOLConst u

instance (Typeable u) => Pretty (SomeHOLConst u) where
  pretty (SomeHOLConst x) = pretty x
instance (Typeable u) => PrettyTyped (SomeHOLConst u) where
  prettyTyped (SomeHOLConst x) = prettyTyped x
instance (Typeable u) => Show (SomeHOLConst u) where
  show = show . prettyTyped

instance Typeable u => Some (HOLConst u) where
  type Something (HOLConst u) = SomeHOLConst u
  gen = SomeHOLConst
  unGen (SomeHOLConst f) = cast f
\end{code}

\begin{code}
data SomeHOLVar u where
  SomeHOLVar :: (Typeable t) => !(HOLVar u t) -> SomeHOLVar u

instance (Typeable u) => Pretty (SomeHOLVar u) where
  pretty (SomeHOLVar x) = pretty x
instance (Typeable u) => PrettyTyped (SomeHOLVar u) where
  prettyTyped (SomeHOLVar x) = prettyTyped x
instance (Typeable t) => Show (SomeHOLVar t) where
  show = show . prettyTyped

instance Typeable u => Some (HOLVar u) where
  type Something (HOLVar u) = SomeHOLVar u
  gen = SomeHOLVar
  unGen (SomeHOLVar f) = cast f
\end{code}

\begin{code}
data SomeHOLTerm u where
  SomeHOLTerm :: (Typeable t) => !(HOLTerm u t) -> SomeHOLTerm u

instance (Typeable u) => Pretty (SomeHOLTerm u) where
  pretty (SomeHOLTerm x) = pretty x
instance (Typeable u) => PrettyTyped (SomeHOLTerm u) where
  prettyTyped (SomeHOLTerm x) = prettyTyped x
instance (Typeable t) => Show (SomeHOLTerm t) where
  show = show . prettyTyped

instance Typeable u => Some (HOLTerm u) where
  type Something (HOLTerm u) = SomeHOLTerm u
  gen = SomeHOLTerm
  unGen (SomeHOLTerm f) = cast f
\end{code}
