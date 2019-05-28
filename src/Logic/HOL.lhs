\begin{code}
{-# LANGUAGE GADTs, KindSignatures, ScopedTypeVariables, ConstraintKinds, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

module Logic.HOL (
  HOLTyped(..),
  ToHOL(..), FromHOL(..),
  HOLVar(..),
  HOLConst(..),
  HOLTerm(..),
  var, constant,
  not, forall, exists, app, lam, definition,
  (.->), (.|), (.&), (.@),
  name, def,
  SomeHOLFormulae(..), gen,
  pretty, prettyTyped,
) where

import Data.Typeable
import Prelude hiding (forall, exists, not)
import qualified Data.Functor.Const as FC
import Data.Text.Prettyprint.Doc

\end{code}

Definition of HOL
* Variables - HOLVar t u
* Constants - HOLConst t u
* Terms - HOLTerm t u
of type "t" and uninterpreted term wrapper "u".
\begin{code}
class HOLTyped t where
  getHOLType :: t -> TypeRep

instance forall t u. (Typeable t, Typeable u) => HOLTyped (HOLVar t u) where
  getHOLType = head . typeRepArgs . typeOf
instance forall t u. (Typeable t, Typeable u) => HOLTyped (HOLConst t u) where
  getHOLType = head . typeRepArgs . typeOf
instance forall t u. (Typeable t, Typeable u) => HOLTyped (HOLTerm t u) where
  getHOLType = head . typeRepArgs . typeOf
\end{code}

Pretty printer for typed constances, variables, terms and so on.
\begin{code}
class PrettyTyped t where
  prettyTyped :: t -> Doc ann
instance PrettyTyped a => PrettyTyped [a] where
  prettyTyped as = list $ map prettyTyped as
\end{code}

\begin{code}
data HOLVar t u where
  HOLVar :: String -> HOLVar t u

data HOLConst t u where
  HOLConst :: String -> HOLConst t u
  HOLUninterpreted :: u -> HOLConst t u
  HOLDef :: String -> HOLTerm t u -> HOLConst t u

data HOLTerm t u where
  T :: Typeable u => HOLTerm Bool u
  F :: Typeable u => HOLTerm Bool u
  Var :: (Typeable t, Typeable u) => HOLVar t u -> HOLTerm t u
  Const :: (Typeable t, Typeable u) => HOLConst t u -> HOLTerm t u
  Not :: Typeable u => HOLTerm Bool u -> HOLTerm Bool u
  And :: Typeable u => HOLTerm Bool u -> HOLTerm Bool u -> HOLTerm Bool u
  Or :: Typeable u => HOLTerm Bool u -> HOLTerm Bool u -> HOLTerm Bool u
  Imply :: Typeable u => HOLTerm Bool u -> HOLTerm Bool u -> HOLTerm Bool u
  Lam :: (Typeable s, Typeable t, Typeable u) => HOLVar s u -> HOLTerm t u -> HOLTerm (s -> t) u
  App :: (Typeable s, Typeable t, Typeable u) => HOLTerm (s -> t) u -> HOLTerm s u -> HOLTerm t u
  Forall :: (Typeable s, Typeable u) => HOLVar s u -> HOLTerm Bool u -> HOLTerm Bool u
  Exists :: (Typeable s, Typeable u) => HOLVar s u -> HOLTerm Bool u -> HOLTerm Bool u

instance Show (HOLVar t u) where
  show = show . pretty
instance Show (HOLConst t u) where
  show = show . pretty

instance Pretty (HOLVar t u) where
  pretty (HOLVar x) = pretty x
instance Pretty (HOLConst t u) where
  pretty (HOLConst x) = pretty x
  pretty (HOLUninterpreted x) = pretty ""
  pretty (HOLDef name x) = pretty name <> pretty ":" <+> pretty x

instance (Typeable t, Typeable u) => PrettyTyped (HOLVar t u) where
  prettyTyped x = pretty x <+> pretty "::" <+> (pretty $ show $ getHOLType x)
instance (Typeable t, Typeable u) => PrettyTyped (HOLConst t u) where
  prettyTyped x = pretty x <+> pretty "::" <+> (pretty $ show $ getHOLType x)
\end{code}

TODO: Use a better Pretty implementation to remove unnessesary "()".
\begin{code}
instance Show (HOLTerm t u) where
  show = show . pretty
instance Pretty (HOLTerm t u) where
  pretty x = case x of
    T -> pretty "T"
    F -> pretty "F"
    (Var var) -> pretty $ show var
    (Const cst) -> pretty $ show cst
    (Not a) -> parens $ pretty "not" <+> pretty a
    (And a b) -> parens $  pretty a <+> pretty "&" <+> pretty b
    (Or a b) -> parens $  pretty a <+> pretty "|" <+> pretty b
    (Imply a b) -> parens $  pretty a <+> pretty "->" <+> pretty b
    (Lam v a) -> pretty "\\" <> pretty v <> pretty "." <+> pretty a
    (App f x) -> parens $ pretty f <> pretty "@" <> pretty x
    (Forall x f) -> pretty "∀" <> pretty x <> pretty ":" <+> pretty f 
    (Exists x f) -> pretty "∃" <> pretty x <> pretty ":" <+> pretty f
instance (Typeable t, Typeable u) => PrettyTyped (HOLTerm t u) where
  prettyTyped x = pretty x <+> pretty "::" <+> (pretty $ show $ getHOLType x)
\end{code}

Conversion from and to HOL-Terms. Mainly used for convinience to omit explicit constructor wrapping.
\begin{code}
class (Typeable t, Typeable u) => ToHOL a t u where
  toHOL :: a t u -> HOLTerm t u

instance (Typeable t, Typeable u) => ToHOL HOLVar t u where
  toHOL = Var
instance (Typeable t, Typeable u) => ToHOL HOLConst t u where
  toHOL c = case c of
    (HOLConst _) -> Const c
    (HOLDef name _) -> Const $ HOLConst name
instance (Typeable t, Typeable u) => ToHOL HOLTerm t u where
  toHOL = id
class (Typeable t, Typeable u) =>  FromHOL a t u where
  fromHOL :: HOLTerm t u -> a t u
instance (Typeable t, Typeable u) => FromHOL HOLTerm t u where
  fromHOL = id
type HOL a t u = (ToHOL a t u, FromHOL a t u)

\end{code}

Shorthands for construction or terms.
\begin{code}
infixl 4 .->
infixl 5 .|
infixl 6 .&
infixl 7 .@

var :: (Typeable t, Typeable u) => String -> HOLVar t u
var = HOLVar
constant :: (Typeable t, Typeable u) => String -> HOLConst t u
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

\end{code}

Get the definition of a constant.
\begin{code}
def :: HOLConst t u -> HOLTerm t u
def (HOLDef _ f) = f
def (HOLConst _) = undefined
\end{code}

Get the name of a constant.
\begin{code}
name :: HOLConst t u -> String
name (HOLDef name _) = name
name (HOLConst name) = name

\end{code}

Helper to generete List over multible types. Allows:
x = constant "x" :: HOLConst (Bool) ()
h = constant "h" :: HOLConst (Bool -> Bool) ()
f = definition "f" $ T .& x
l = [gen x, gen h, gen f]
\begin{code}
data SomeHOLFormulae u where
  SomeHOLFormulae :: (Typeable t) => !(HOLConst t u) -> SomeHOLFormulae u
gen :: (Typeable t) => (HOLConst t u) -> SomeHOLFormulae u
gen = SomeHOLFormulae

instance (Typeable t, Show t) => Show (SomeHOLFormulae t) where
  show = show . pretty

instance (Typeable u) => Pretty (SomeHOLFormulae u) where
  pretty (SomeHOLFormulae x) = pretty x
instance (Typeable u) => PrettyTyped (SomeHOLFormulae u) where
  prettyTyped (SomeHOLFormulae x) = prettyTyped x
\end{code}
