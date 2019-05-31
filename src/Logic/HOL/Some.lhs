
\begin{code}
{-# LANGUAGE GADTs, TypeFamilies, FlexibleContexts, ScopedTypeVariables, AllowAmbiguousTypes #-}

module Logic.HOL.Some where

import Logic.HOL.Internal

import Data.Typeable
import Prelude hiding (forall, exists, not)
import Data.Text.Prettyprint.Doc

class Some s where
  type Something s
  gen :: s -> Something s
  unGen :: Something s -> Maybe s
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

instance (Typeable u, Typeable t) => Some (HOLConst u t) where
  type Something (HOLConst u t) = SomeHOLConst u
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

instance (Typeable u, Typeable t) => Some (HOLVar u t) where
  type Something (HOLVar u t) = SomeHOLVar u
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

instance (Typeable u, Typeable t) => Some (HOLTerm u t) where
  type Something (HOLTerm u t) = SomeHOLTerm u
  gen = SomeHOLTerm
  unGen (SomeHOLTerm f) = cast f
\end{code}

\subsection{construction of terms of some wrapped typed}
\begin{code}
infixl 4 ..->

(..->) :: forall u. Typeable u => SomeHOLTerm u -> SomeHOLTerm u -> Maybe (SomeHOLTerm u)
a ..-> b = case (unGen a :: Maybe (HOLTerm u Bool), unGen b :: Maybe (HOLTerm u Bool)) of
  (Just a', Just b') -> Just $ gen $ (a' .-> b')
  _ -> Nothing

(..@) :: forall u t s. (Typeable u, Typeable t, Typeable s) => SomeHOLTerm u -> SomeHOLTerm u -> Maybe (SomeHOLTerm u)
f ..@ x = case (unGen f, unGen x) :: (Maybe (HOLTerm u (s -> t)), Maybe (HOLTerm u s)) of
  (Just f', Just x') -> Just $ gen $ f' .@ x'
  _ -> Nothing
\end{code}