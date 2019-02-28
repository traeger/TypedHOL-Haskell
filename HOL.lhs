\begin{code}
{-# LANGUAGE TypeOperators, DataKinds, GADTs, KindSignatures, ScopedTypeVariables, ConstraintKinds #-}

module HOL where

import Data.Typeable

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
infixl 5 :$
infixl 5 :$$

infixl 5 :@:, :&:

data HOLType = I | O | HOLType :$ HOLType deriving (Eq, Ord)
instance Show HOLType where
  show I = "I"
  show O = "O"
  show (x :$ O) = show x ++ "O"
  show (x :$ I) = show x ++ "I"
  show (x :$ y) = show x ++ "(" ++ show y ++ ")"

\end{code}
Mapping a Type to a recursive GADT-representation of the type.
"HOLTypeRep t" GADT representation of "HOLType".

The trick lies in:
  "class HOLTyped (t :: HOLType) where"
  "  getHOLType :: proxy t -> HOLTypeRep t"
which allows, to map ANY type paramerized with "t" to its GADT representation "HOLTypeRep t".
\begin{code}
data HOLTypeRep (t :: HOLType) where
  RI :: HOLTypeRep I
  RO :: HOLTypeRep O
  (:$$) :: HOLTypeRep t -> HOLTypeRep s -> HOLTypeRep (t :$ s)
instance Show (HOLTypeRep t) where
  show RI = "I"
  show RO = "O"
  show (t :$$ RO) = show t ++ "O"
  show (t :$$ RI) = show t ++ "I"
  show (t :$$ s) = show t ++ "(" ++ show s ++ ")"

class HOLTyped (t :: HOLType) where
  getHOLType :: proxy t -> HOLTypeRep t
instance HOLTyped I where
  getHOLType _ = RI
instance HOLTyped O where
  getHOLType _ = RO
instance (HOLTyped s, HOLTyped t) => HOLTyped (t :$ s) where
  getHOLType _ = (getHOLType undefined) :$$ (getHOLType undefined)
\end{code}

\begin{code}
data HOLVar (t :: HOLType) where
  HOLVar :: String -> HOLVar t 

data HOLTerm (t :: HOLType) where
  T :: HOLTerm O
  F :: HOLTerm O
  Var :: HOLVar t -> HOLTerm t
  Not :: HOLTerm O -> HOLTerm O
  (:&:) :: HOLTerm O -> HOLTerm O -> HOLTerm O
  (:|:) :: HOLTerm O -> HOLTerm O -> HOLTerm O
  (:->:) :: HOLTerm O -> HOLTerm O -> HOLTerm O
  Lam :: HOLVar s -> HOLTerm t -> HOLTerm (t :$ s)
  (:@:) :: HOLTerm (t :$ s) -> HOLTerm s -> HOLTerm t

instance Show (HOLVar t) where
  show (HOLVar x) = x

instance Show (HOLTerm t) where
  show x = case x of
    T -> "T"
    F -> "F"
    (Var var) -> show var
    (Not a) -> "(not " ++ "a" ++ ")"
    (a :&: b) -> "(" ++ (show a) ++ " & " ++ (show b) ++ ")"
    (a :|: b) -> "(" ++ (show a) ++ " | " ++ (show b) ++ ")"
    (a :->: b) -> "(" ++ (show a) ++ " -> " ++ (show b) ++ ")"
    (Lam v a) -> "\\" ++ (show v) ++ ": " ++ (show a)
    (f :@: x) -> (show f) ++ "@" ++ (show x) 

\end{code}

Conversion from and to HOL-Terms.
\begin{code}
class ToHOL a where
  toHOL :: forall (t :: HOLType). a t -> HOLTerm t
instance ToHOL HOLVar where
  toHOL = Var
instance ToHOL HOLTerm where
  toHOL = id
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
lam v a = Lam v (toHOL a)
\end{code}

Examples
\begin{code}  
x :: HOLVar O
x = HOLVar "x"

y :: HOLVar I
y = HOLVar "y"

f :: HOLVar (O :$ I)
f = HOLVar "f"
\end{code}