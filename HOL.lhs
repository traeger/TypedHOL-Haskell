\begin{code}
{-# LANGUAGE GADTs, KindSignatures, ScopedTypeVariables, ConstraintKinds, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

module HOL where

import Data.Typeable
import Prelude hiding (forall, exists)
import qualified Data.Functor.Const as FC

infixl 4 .->
infixl 5 .|
infixl 6 .&
infixl 7 .@
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
  show (HOLVar x) = x

instance Show (HOLConst t u) where
  show (HOLConst x) = x
  show (HOLUninterpreted x) = ""
  show (HOLDef name x) = name ++ ": " ++ (show x)

instance Show (HOLTerm t u) where
  show x = case x of
    T -> "T"
    F -> "F"
    (Var var) -> show var
    (Const cst) -> show cst
    (Not a) -> "(not " ++ "a" ++ ")"
    (And a b) -> "(" ++ (show a) ++ " & " ++ (show b) ++ ")"
    (Or a b) -> "(" ++ (show a) ++ " | " ++ (show b) ++ ")"
    (Imply a b) -> "(" ++ (show a) ++ " -> " ++ (show b) ++ ")"
    (Lam v a) -> "\\" ++ (show v) ++ ". " ++ (show a)
    (App f x) -> "(" ++ (show f) ++ "@" ++ (show x) ++ ")"
    (Forall x f) -> "∀" ++ (show x) ++ ":" ++ (show f) 
    (Exists x f) -> "∃" ++ (show x) ++ ":" ++ (show f)

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
data SomeHOLConst u where
  SomeHOLConst :: (Typeable t) => !(HOLConst t u) -> SomeHOLConst u
gen :: (Typeable t) => (HOLConst t u) -> SomeHOLConst u
gen = SomeHOLConst
\end{code}
