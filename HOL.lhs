\begin{code}
{-# LANGUAGE TypeOperators, DataKinds, GADTs, KindSignatures, ScopedTypeVariables, ConstraintKinds #-}

module HOL where

import Data.Typeable
import Prelude hiding (forall, exists)

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

instance Typeable t => HOLTyped (HOLVar t) where
  getHOLType = head . typeRepArgs . typeOf
instance Typeable t => HOLTyped (HOLTerm t) where
  getHOLType = head . typeRepArgs . typeOf
instance Typeable t => HOLTyped (HOLDef t) where
  getHOLType = head . typeRepArgs . typeOf
\end{code}

\begin{code}
data HOLVar t where
  HOLVar :: String -> HOLVar t

data HOLConst t where
  HOLConst :: String -> HOLConst t

data HOLTerm t where
  T :: HOLTerm Bool
  F :: HOLTerm Bool
  Var :: Typeable t => HOLVar t -> HOLTerm t
  Const :: Typeable t => HOLConst t -> HOLTerm t
  Not :: HOLTerm Bool -> HOLTerm Bool
  (:&:) :: HOLTerm Bool -> HOLTerm Bool -> HOLTerm Bool
  (:|:) :: HOLTerm Bool -> HOLTerm Bool -> HOLTerm Bool
  (:->:) :: HOLTerm Bool -> HOLTerm Bool -> HOLTerm Bool
  Lam :: (Typeable s, Typeable t) => HOLVar s -> HOLTerm t -> HOLTerm (s -> t)
  (:@:) :: (Typeable s, Typeable t) => HOLTerm (s -> t) -> HOLTerm s -> HOLTerm t
  Forall :: Typeable s => HOLVar s -> HOLTerm Bool -> HOLTerm Bool
  Exists :: Typeable s => HOLVar s -> HOLTerm Bool -> HOLTerm Bool

data HOLDef t where
  HOLDefTerm :: String -> HOLTerm t -> HOLDef t
  HOLDefConst :: HOLConst t -> HOLDef t

instance Show (HOLDef t) where
  show (HOLDefTerm name t) = "Def: " ++ name ++ " = " ++ (show t)
  show (HOLDefConst v) = "Const: " ++ (show v)

-- for generic lists:
data SomeHOLDef where
  SomeHOLDef :: (Typeable t) => !(HOLDef t) -> SomeHOLDef
gen :: (Typeable t) => (HOLDef t) -> SomeHOLDef
gen = SomeHOLDef

instance Show (HOLVar t) where
  show (HOLVar x) = x

instance Show (HOLConst t) where
  show (HOLConst x) = x

instance Show (HOLTerm t) where
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
  toHOL :: forall t. Typeable t => a t -> HOLTerm t
instance ToHOL HOLVar where
  toHOL = Var
instance ToHOL HOLConst where
  toHOL = Const
instance ToHOL HOLTerm where
  toHOL = id
instance ToHOL HOLDef where
  toHOL (HOLDefTerm name t) = Const $ HOLConst name -- enforce type of t, make it a const
  toHOL (HOLDefConst cst) = Const $ cst -- make it a const
class FromHOL a where
  fromHOL :: HOLTerm t -> a t
instance FromHOL HOLTerm where
  fromHOL = id
type HOL a = (ToHOL a, FromHOL a)

\end{code}

Shorthands for construction or terms.
\begin{code}

not a = Not (toHOL a)
a .& b = (toHOL a) :&: (toHOL b)
a .| b = (toHOL a) :|: (toHOL b)
a .-> b = (toHOL a) :->: (toHOL b)
f .@ x = (toHOL f) :@: (toHOL x)
app f = (.@) f
lam v a = Lam v (toHOL a)
forall x f = Forall x (toHOL f)
exists x f = Exists x (toHOL f)
definition s f = HOLDefTerm s f
constant cst = HOLDefConst cst

def (HOLDefTerm _ f) = f
name (HOLDefTerm n _) = n
name (HOLDefConst (HOLConst n)) = n
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
