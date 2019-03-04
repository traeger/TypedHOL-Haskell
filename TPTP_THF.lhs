\begin{code}
{-# LANGUAGE ViewPatterns, TypeFamilies #-}

module TPTP_THF where

import Data.Typeable
import HOL
import Prelude hiding (forall, exists)

toTHF :: Typeable t => HOLDef t -> [String]
toTHF (HOLDef name term) = 
  ["thf(" ++ name ++ "_decl,type,(" ++ name ++ ":" ++ (toTHFType term) ++ "))."
  ,"thf(" ++ name ++ ",definition,(" ++ name ++ "=(" ++ (toTHFTerm term) ++ "))."]
toTHF (HOLConj name term) = 
  ["thf(" ++ name ++ ",conjecture,(" ++ name ++ "=(" ++ (toTHFTerm term) ++ "))."]

toTHFType :: Typeable t => HOLTerm t -> String
toTHFType term = toTHFType' $ getHOLType term where
  toTHFType' x = case splitTyConApp x of
    (_,[y,z]) -> (toTHFType' y) ++ " > " ++ (toTHFType' z) -- TODO: add (->) to pattern match, but how?
    (show -> "Bool", [])   -> "$o"
    (_, [])   -> "$i"

--  toTHFType' (Fun a b) = (toTHFType' a) ++ " > " ++ (toTHFType' b)

toTHFTerm :: Typeable t => HOLTerm t -> String
toTHFTerm term = case term of
  T -> "$true"
  F -> "$false"
  Var (HOLVar name) -> name
  Not a -> "" ++ (toTHFTerm a) -- TODO: figure out what not is
  a :&: b -> "(" ++ (toTHFTerm a) ++ "&" ++ (toTHFTerm b) ++ ")"
  a :|: b -> "(" ++ (toTHFTerm a) ++ "|" ++ (toTHFTerm b) ++ ")"
  a :->: b -> "(" ++ (toTHFTerm a) ++ "=>" ++ (toTHFTerm b) ++ ")"
  Lam x a -> "( ^[" ++ (name $ Var x) ++ ": " ++ (toTHFType $ Var x) ++ "]: " ++ (toTHFTerm a) ++ " )"
  a :@: x -> "(" ++ (toTHFTerm a) ++ "@" ++ (toTHFTerm x) ++ ")"
  Forall x a -> "( ![" ++ (name $ Var x) ++ ": " ++ (toTHFType $ Var x) ++ "]: " ++ (toTHFTerm a) ++ " )"
  Exists x a -> "( ![" ++ (name $ Var x) ++ ": " ++ (toTHFType $ Var x) ++ "]: " ++ (toTHFTerm a) ++ " )" -- TODO: figure out what not is
  Def a -> name $ Def a
  
z = splitTyConApp

\end{code}