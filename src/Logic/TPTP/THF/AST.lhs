\section{THF AST}

\subsection{Usage}

\begin{code}
module Logic.TPTP.THF.AST where

import Data.Map (Map)
import Data.Map as Map

data HOLType
  = HOLBaseType String
  | HOLVarType String
  | HOLFuncType HOLType HOLType deriving (Show, Eq, Ord)

data HOLVar
  = HOLVar HOLType String deriving (Show, Eq, Ord)

data HOLConst
  = HOLConst HOLType String
  | HOLDef HOLType String HOLTerm deriving (Show, Eq, Ord)

data HOLTerm
  = T
  | F
  | Var    HOLVar
  | Const  HOLConst
  | Not    HOLTerm
  | And    HOLTerm HOLTerm
  | Or     HOLTerm HOLTerm
  | Imply  HOLTerm HOLTerm
  | App    HOLTerm HOLTerm
  | Lam    [HOLVar] HOLTerm
  | Forall [HOLVar] HOLTerm
  | Exists [HOLVar] HOLTerm
  | Equal  HOLTerm HOLTerm deriving (Show, Eq, Ord)

insertVar :: HOLVar -> Map String HOLVar -> Map String HOLVar
insertConst :: HOLConst -> Map String HOLConst -> Map String HOLConst

insertVar v@(HOLVar t name) = insert name v
insertConst c@(HOLConst t name) = insert name c

mkVarMap :: [HOLVar] -> Map String HOLVar
mkConstMap :: [HOLConst] -> Map String HOLConst

mkVarMap [] = Map.empty
mkVarMap (x:xs) = insertVar x (mkVarMap xs)

mkConstMap [] = Map.empty
mkConstMap (x:xs) = insertConst x (mkConstMap xs)
\end{code}

\begin{code}
data AbstractTHFType 
  = AbstractTHFType String String deriving (Show, Eq, Ord)

data AbstractTHFFormulae
  = AbstractTHFAxiom String String
  | AbstractTHFDefinition String String
  | AbstractTHFConjecture String String deriving (Show, Eq, Ord)

data AbstractTHFFile =
  AbstractTHFFile [AbstractTHFType] [AbstractTHFFormulae] deriving (Show, Eq, Ord)
\end{code}
