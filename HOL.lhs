\begin{code}
{-# LANGUAGE TypeOperators, DataKinds, GADTs, KindSignatures, ScopedTypeVariables #-}

module HOL where

import Data.Typeable

\end{code}
Modelling HOL Types like:

$o$ :: Literal Bool Type
$i$ :: Literal Individual Type
$o :\$ o$ short $oo$ :: Function Type: Bool -> Bool
$o :\$ i :\$ i$ short $iio$ :: Function Type: Individual -> Individual -> Bool
$i :\$ i$ short $ii$ :: Function Type: Individual -> Individual
$i :\$ (i :\$ i)$ short $i(ii)$ :: Function Type: (Individual -> Individual) -> Individual
$i :\$ (o :\$ i)$ short $i(io)$ :: Function Type: (Individual -> Bool) -> Individual
\begin{code}
infixl 5 :$
infixl 5 :$$

data HOLType = I | O | HOLType :$ HOLType deriving (Eq, Ord)

\end{code}
Mapping a Type to a recursive GADT-representation of the type.
"HOLTypeRepl t" GADT representation of "HOLType".

The trick lies in:
  "class HOLTyped (t :: HOLType) where"
  "  getHOLType :: proxy t -> HOLTypeRepl t"
which allows, to map ANY type paramerized with "t" to its GADT representation "HOLTypeRepl t".
\begin{code}
data HOLTypeRepl (t :: HOLType) where
  RI :: HOLTypeRepl I
  RO :: HOLTypeRepl O
  (:$$) :: HOLTypeRepl t -> HOLTypeRepl s -> HOLTypeRepl (t :$ s)

instance Show (HOLTypeRepl t) where
  show RI = "I"
  show RO = "O"
  show (t :$$ RO) = show t ++ "O"
  show (t :$$ RI) = show t ++ "I"
  show (t :$$ s) = show t ++ "(" ++ show s ++ ")"

class HOLTyped (t :: HOLType) where
  getHOLType :: proxy t -> HOLTypeRepl t
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
  (:&:) :: HOLTerm O -> HOLTerm O -> HOLTerm O
  Lam :: HOLVar s -> HOLTerm t -> HOLTerm (t :$ s)

instance Show HOLType where
  show I = "I"
  show O = "O"
  show (x :$ O) = show x ++ "O"
  show (x :$ I) = show x ++ "I"
  show (x :$ y) = show x ++ "(" ++ show y ++ ")"

instance Show (HOLVar t) where
  show (HOLVar x) = x

instance Show (HOLTerm t) where
  show x = case x of
    T -> "T"
    F -> "F"
    (Var var) -> show var
    (a :&: b) -> (show a) ++ " & " ++ (show b)
    (Lam v a) -> "\\" ++ (show v) ++ ": " ++ (show a)
  
x :: HOLVar O
x = HOLVar "x"

y :: HOLVar I
y = HOLVar "y"

f :: HOLVar (O :$ I)
f = HOLVar "f"
\end{code}