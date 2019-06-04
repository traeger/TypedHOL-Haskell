
\begin{code}
{-# LANGUAGE GADTs, TypeFamilies, FlexibleContexts, ExistentialQuantification #-}

module Logic.HOL.Internal.Some where

import Logic.HOL.Internal

import Data.Typeable
import Prelude hiding (forall, exists, not)
import Data.Text.Prettyprint.Doc

class Some s where
  type Something s
  gen :: Typeable t => s t -> Something s
  unGen :: Typeable t => Something s -> Maybe (s t)
  unGenJust :: Typeable t => Something s -> s t
  
  unGenJust x = case unGen x of
    Just x' -> x'
  
-- type convertions
convert0 :: (Typeable s, Typeable t, Typeable v) => s t -> s v
convert1 :: (Typeable s, Typeable a, Typeable b, Typeable t, Typeable v) => (s a -> s b) -> s t -> s v
convert2 :: (Typeable s, Typeable a, Typeable b, Typeable c, Typeable t, Typeable v, Typeable w) => (s a -> s b -> s c) -> s t -> s v -> s w
convert2f :: (Typeable s, Typeable a, Typeable b, Typeable t, Typeable v, Typeable w) => (s (a -> b) -> s a -> s b) -> s t -> s v -> s w

convert0 x = case cast x of
  Just x' -> x'
convert1 f = convert0 . f . convert0
convert2 f a b = convert0 $ f (convert0 a) (convert0 b)
convert2f f a b = convert0 $ f (convert0 a) (convert0 b)
\end{code}

Helper to generete List over multible types. Allows:
\begin{code}# example
x = constant "x" :: HOLConst () (Bool) 
h = constant "h" :: HOLConst () (Bool -> Bool) 
f = definition "f" \$ T .& x
l = [gen x, gen h, gen f]
\end{code}# example

\begin{code}
data SomeHOLConst u = forall self. Typeable self => SomeHOLConst
  { _const    :: HOLConst u self
  }

instance (Typeable u) => Pretty (SomeHOLConst u) where
  pretty (SomeHOLConst x) = pretty x
instance (Typeable u) => PrettyTyped (SomeHOLConst u) where
  prettyTyped (SomeHOLConst x) = prettyTyped x
instance (Typeable u) => Show (SomeHOLConst u) where
  show = show . prettyTyped

instance (Typeable u) => Some (HOLConst u) where
  type Something (HOLConst u) = SomeHOLConst u
  gen = SomeHOLConst
  unGen (SomeHOLConst f) = cast f
\end{code}

\begin{code}
data SomeHOLVar u = forall self. Typeable self => SomeHOLVar
  { _var    :: HOLVar u self
  }

instance (Typeable u) => Pretty (SomeHOLVar u) where
  pretty (SomeHOLVar x) = pretty x
instance (Typeable u) => PrettyTyped (SomeHOLVar u) where
  prettyTyped (SomeHOLVar x) = prettyTyped x
instance (Typeable t) => Show (SomeHOLVar t) where
  show = show . prettyTyped

instance (Typeable u) => Some (HOLVar u) where
  type Something (HOLVar u) = SomeHOLVar u
  gen = SomeHOLVar
  unGen (SomeHOLVar f) = cast f
\end{code}

\begin{code}
data SomeHOLTerm u = forall self. Typeable self => SomeHOLTerm
  { _term    :: HOLTerm u self
  }

instance (Typeable u) => Pretty (SomeHOLTerm u) where
  pretty (SomeHOLTerm x) = pretty x
instance (Typeable u) => PrettyTyped (SomeHOLTerm u) where
  prettyTyped (SomeHOLTerm x) = prettyTyped x
instance (Typeable t) => Show (SomeHOLTerm t) where
  show = show . prettyTyped

instance (Typeable u) => Some (HOLTerm u) where
  type Something (HOLTerm u) = SomeHOLTerm u
  gen = SomeHOLTerm
  unGen (SomeHOLTerm f) = cast f
\end{code}
