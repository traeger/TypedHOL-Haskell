\begin{code}
{-# LANGUAGE GADTs, KindSignatures, ScopedTypeVariables, ConstraintKinds, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

module Logic.HOL.Internal where

import Data.Typeable
import Prelude hiding (forall, exists, not)
import qualified Data.Functor.Const as FC
import Data.Text.Prettyprint.Doc

\end{code}

Definition of HOL
* Variables - HOLVar u t
* Constants - HOLConst u t
* Terms - HOLTerm u t
of type "t" and uninterpreted term wrapper "u".
\begin{code}
class HOLTyped t where
  getHOLType :: t -> TypeRep

instance forall u t. (Typeable t, Typeable u) => HOLTyped (HOLVar u t) where
  getHOLType = head . tail . typeRepArgs . typeOf
instance forall u t. (Typeable t, Typeable u) => HOLTyped (HOLConst u t) where
  getHOLType = head . tail . typeRepArgs . typeOf
instance forall u t. (Typeable t, Typeable u) => HOLTyped (HOLTerm u t) where
  getHOLType = head . tail . typeRepArgs . typeOf
\end{code}

Pretty printer for typed constances, variables, terms and so on.
\begin{code}
class PrettyTyped t where
  prettyTyped :: t -> Doc ann
\end{code}

\begin{code}
data HOLVar u t where
  HOLVar :: String -> HOLVar u t

data HOLConst u t where
  HOLConst :: String -> HOLConst u t
  HOLUninterpreted :: u -> HOLConst u t
  HOLDef :: String -> HOLTerm u t -> HOLConst u t

-- TODO add binder style for forall and exists? 
-- hence, change the type to:
-- (Typeable s, Typeable u) => HOLVar s u -> HOLTerm (s -> Bool) u -> HOLTerm Bool u
data HOLTerm u t where
  T :: Typeable u => HOLTerm u Bool
  F :: Typeable u => HOLTerm u Bool
  Var :: (Typeable t, Typeable u) => HOLVar u t -> HOLTerm u t
  Const :: (Typeable t, Typeable u) => HOLConst u t -> HOLTerm u t
  Not :: Typeable u => HOLTerm u Bool -> HOLTerm u Bool
  And :: Typeable u => HOLTerm u Bool -> HOLTerm u Bool -> HOLTerm u Bool
  Or :: Typeable u => HOLTerm u Bool -> HOLTerm u Bool -> HOLTerm u Bool
  Imply :: Typeable u => HOLTerm u Bool -> HOLTerm u Bool -> HOLTerm u Bool
  App :: (Typeable s, Typeable t, Typeable u) => HOLTerm u (s -> t) -> HOLTerm u s -> HOLTerm u t
  Lam :: (Typeable s, Typeable t, Typeable u) => HOLVar u s -> HOLTerm u t -> HOLTerm u (s -> t)
  Forall :: (Typeable s, Typeable u) => HOLVar u s -> HOLTerm u Bool -> HOLTerm u Bool
  Exists :: (Typeable s, Typeable u) => HOLVar u s -> HOLTerm u Bool -> HOLTerm u Bool
  Equal :: (Typeable t, Typeable u) => HOLTerm u t -> HOLTerm u t -> HOLTerm u Bool

instance Pretty (HOLVar u t) where
  pretty (HOLVar x) = pretty x
instance Pretty (HOLConst u t) where
  pretty (HOLConst x) = pretty x
  pretty (HOLUninterpreted x) = pretty ""
  pretty (HOLDef name x) = pretty name <> pretty ":" <+> pretty x

instance (Typeable t, Typeable u) => PrettyTyped (HOLVar u t) where
  prettyTyped x = pretty x <+> pretty "::" <+> (pretty $ show $ getHOLType x)
instance (Typeable t, Typeable u) => PrettyTyped (HOLConst u t) where
  prettyTyped x = pretty x <+> pretty "::" <+> (pretty $ show $ getHOLType x)

instance (Typeable t, Typeable u) => Show (HOLVar u t) where
  show = show . prettyTyped
instance (Typeable t, Typeable u) => Show (HOLConst u t) where
  show = show . prettyTyped
\end{code}

TODO: Use a better Pretty implementation to remove unnessesary "()".
\begin{code}
instance Pretty (HOLTerm u t) where
  pretty x = case x of
    T -> pretty "T"
    F -> pretty "F"
    (Var var) -> pretty var
    (Const cst) -> pretty cst
    (Not a) -> parens $ pretty "not" <+> pretty a
    (And a b) -> parens $  pretty a <+> pretty "&" <+> pretty b
    (Or a b) -> parens $  pretty a <+> pretty "|" <+> pretty b
    (Imply a b) -> parens $  pretty a <+> pretty "->" <+> pretty b
    (Lam v a) -> pretty "\\" <> pretty v <> pretty "." <+> pretty a
    (App f x) -> parens $ pretty f <> pretty "@" <> pretty x
    (Forall x f) -> pretty "∀" <> pretty x <> pretty ":" <+> pretty f 
    (Exists x f) -> pretty "∃" <> pretty x <> pretty ":" <+> pretty f
    (Equal x y) -> pretty x <> pretty "=" <> pretty y

instance (Typeable t, Typeable u) => PrettyTyped (HOLTerm u t) where
  prettyTyped x = pretty x <+> pretty "::" <+> (pretty $ show $ getHOLType x)

instance (Typeable t, Typeable u) => Show (HOLTerm u t) where
  show = show . prettyTyped
\end{code}

Conversion from and to HOL-Terms. Mainly used for convinience to omit explicit constructor wrapping.
\begin{code}
class (Typeable t, Typeable u) => ToHOL a u t where
  toHOL :: a u t -> HOLTerm u t

instance (Typeable t, Typeable u) => ToHOL HOLVar u t where
  toHOL = Var
instance (Typeable t, Typeable u) => ToHOL HOLConst u t where
  toHOL c = case c of
    (HOLConst _) -> Const c
    (HOLDef name _) -> Const $ HOLConst name
instance (Typeable t, Typeable u) => ToHOL HOLTerm u t where
  toHOL = id
class (Typeable t, Typeable u) =>  FromHOL a u t where
  fromHOL :: HOLTerm u t -> a u t
instance (Typeable t, Typeable u) => FromHOL HOLTerm u t where
  fromHOL = id
type HOL a u t = (ToHOL a u t, FromHOL a u t)

\end{code}

Shorthands for construction or terms.
\begin{code}
infixl 8 .=
infixl 7 .@
infixl 6 .&
infixl 5 .|
infixl 4 .->

var :: (Typeable t, Typeable u) => String -> HOLVar u t
var = HOLVar
constant :: (Typeable t, Typeable u) => String -> HOLConst u t
constant cst = HOLConst cst

not a = Not (toHOL a)
a .& b = And (toHOL a) (toHOL b)
a .| b = Or (toHOL a) (toHOL b)
a .-> b = Imply (toHOL a) (toHOL b)
f .@ x = App (toHOL f) (toHOL x)
app f = (.@) f
lam v a = Lam v (toHOL a)
forall x f = Forall x (toHOL f)
exists x f = Exists x (toHOL f)
definition s f = HOLDef s f
x .= y = Equal (toHOL x) (toHOL y)

\end{code}

Get the definition of a constant.
\begin{code}
def :: HOLConst u t -> HOLTerm u t
def (HOLDef _ f) = f
def (HOLConst _) = undefined
\end{code}

Get the name of a constant.
\begin{code}
name :: HOLConst u t -> String
name (HOLDef name _) = name
name (HOLConst name) = name

\end{code}
