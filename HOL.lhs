\begin{code}
{-# LANGUAGE TypeOperators, GADTs, KindSignatures, ScopedTypeVariables, ConstraintKinds #-}

module HOL where

import Data.Typeable
import Prelude hiding (forall, exists)
import qualified Data.Functor.Const as FC

\end{code}
Modelling HOL Types like:

$o$ :: Literal Bool Type
$i$ :: Literal Individual Type
$o :\$ o$ short $oo$ :: Function Type: Bool -> Bool
$o :\$ i :\$ i$ short $oii$ :: Function Type: Individual -> Individual -> Bool
$i :\$ i$ short $ii$ :: Function Type: Individual -> Individual
$i :\$ (i :\$ i)$ short $i(ii)$ :: Function Type: (Individual -> Individual) -> Individual
$i :\$ (o :\$ i)$ short $i(oi)$ :: Function Type: (Individual -> Bool) -> Individual
\begin{code}
infixl 5 :@:, :&:
infixl 4 .->
infixl 5 .|
infixl 6 .&
infixl 7 .@

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
  (:&:) :: Typeable u => HOLTerm Bool u -> HOLTerm Bool u -> HOLTerm Bool u
  (:|:) :: Typeable u => HOLTerm Bool u -> HOLTerm Bool u -> HOLTerm Bool u
  (:->:) :: Typeable u => HOLTerm Bool u -> HOLTerm Bool u -> HOLTerm Bool u
  Lam :: (Typeable s, Typeable t, Typeable u) => HOLVar s u -> HOLTerm t u -> HOLTerm (s -> t) u
  (:@:) :: (Typeable s, Typeable t, Typeable u) => HOLTerm (s -> t) u -> HOLTerm s u -> HOLTerm t u
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
    (a :&: b) -> "(" ++ (show a) ++ " & " ++ (show b) ++ ")"
    (a :|: b) -> "(" ++ (show a) ++ " | " ++ (show b) ++ ")"
    (a :->: b) -> "(" ++ (show a) ++ " -> " ++ (show b) ++ ")"
    (Lam v a) -> "\\" ++ (show v) ++ ". " ++ (show a)
    (f :@: x) -> "(" ++ (show f) ++ "@" ++ (show x) ++ ")"
    (Forall x f) -> "∀" ++ (show x) ++ ":" ++ (show f) 
    (Exists x f) -> "∃" ++ (show x) ++ ":" ++ (show f)

\end{code}

Conversion from and to HOL-Terms.
\begin{code}
class ToHOL a where
  toHOL :: forall t u. (Typeable t, Typeable u) => a t u -> HOLTerm t u

instance ToHOL HOLVar where
  toHOL = Var
instance ToHOL HOLConst where
  toHOL (HOLConst x) = Const $ HOLConst x
  toHOL (HOLDef name x) = Const $ HOLConst name
instance ToHOL HOLTerm where
  toHOL = id
class FromHOL a where
  fromHOL :: HOLTerm t u -> a t u
instance FromHOL HOLTerm where
  fromHOL = id
type HOL a = (ToHOL a, FromHOL a)

\end{code}

Shorthands for construction or terms.
\begin{code}

var :: (Typeable t, Typeable u) => String -> HOLVar t u
var = HOLVar
constant :: (Typeable t, Typeable u) => String -> HOLConst t u
constant cst = HOLConst cst

not a = Not (toHOL a)
a .& b = (toHOL a) :&: (toHOL b)
a .| b = (toHOL a) :|: (toHOL b)
a .-> b = (toHOL a) :->: (toHOL b)
f .@ x = (toHOL f) :@: (toHOL x)
app f = (.@) f
lam v a = Lam v (toHOL a)
forall x f = Forall x (toHOL f)
exists x f = Exists x (toHOL f)
definition s f = HOLDef s f

def :: HOLConst t u -> HOLTerm t u
def (HOLDef _ f) = f
def (HOLConst _) = undefined

name :: HOLConst t u -> String
name (HOLDef name _) = name
name (HOLConst name) = name

-- for generic lists:
data SomeHOLConst u where
  SomeHOLConst :: (Typeable t) => !(HOLConst t u) -> SomeHOLConst u
gen :: (Typeable t) => (HOLConst t u) -> SomeHOLConst u
gen = SomeHOLConst
\end{code}

Examples

x = HOLVar "x" :: HOLVar Bool
y = HOLVar "y" :: HOLVar Int

g0 = HOLVar "g" :: HOLVar (Bool -> Bool)
h0 = HOLVar "h" :: HOLVar (Int -> Int)

f0 = HOLVar "f" :: HOLVar (Int -> Bool)
f1 = HOLVar "f" :: HOLVar (Int -> Int -> Bool)

ax0 :: (ToHOL a2, ToHOL a1) => a1 (s -> t) -> a2 s -> HOLTerm t
ax0 = \f x -> f .@ x
