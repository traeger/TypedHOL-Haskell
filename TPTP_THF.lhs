\begin{code}
{-# LANGUAGE ViewPatterns, TypeFamilies, RankNTypes #-}

module TPTP_THF where

import HOL

import Data.Typeable
import qualified Data.Char as Char

import Prelude hiding (forall, exists)

toTHF :: Typeable t => HOLDef t -> [String]
toTHF (HOLDef name term) = 
  ["thf(" ++ name ++ "_decl,type,(" ++ name ++ ":" ++ (toTHFType term) ++ "))."
  ,"thf(" ++ name ++ ",definition,(" ++ name ++ "=(" ++ (toTHFTerm term) ++ ")))."]
toTHF (HOLConj name term) = 
  ["thf(" ++ name ++ ",conjecture,(" ++ name ++ "=(" ++ (toTHFTerm term) ++ ")))."]
toTHF (HOLConst var) = 
  ["thf(" ++ (toTHFVarName var) ++ ",type,(" ++ (toTHFVarName var) ++ ":" ++ (toTHFType $ Var var) ++ "))."]

toTHFType :: Typeable t => HOLTerm t -> String
toTHFType term = toTHFType' $ getHOLType term where
  toTHFType' x = case splitTyConApp x of
    (_,[y,z]) -> (toTHFType' y) ++ " > " ++ (toTHFType' z) -- TODO: add (->) to pattern match, but how?
    (show -> "Bool", [])   -> "$o"
    (_, [])   -> "$i"

-- TPTP vars need to be upper case
toTHFVarName :: HOLVar t -> String
toTHFVarName (HOLVar name) = name

--  toTHFType' (Fun a b) = (toTHFType' a) ++ " > " ++ (toTHFType' b)

toTHFTerm :: Typeable t => HOLTerm t -> String
toTHFTerm term = case term of
  T -> "$true"
  F -> "$false"
  Var x -> toTHFVarName x
  Not a -> "" ++ (toTHFTerm a) -- TODO: figure out what not is
  a :&: b -> "(" ++ (toTHFTerm a) ++ "&" ++ (toTHFTerm b) ++ ")"
  a :|: b -> "(" ++ (toTHFTerm a) ++ "|" ++ (toTHFTerm b) ++ ")"
  a :->: b -> "(" ++ (toTHFTerm a) ++ "=>" ++ (toTHFTerm b) ++ ")"
  Lam x a -> "( ^[" ++ (toTHFVarName x) ++ ": " ++ (toTHFType $ Var x) ++ "]: " ++ (toTHFTerm a) ++ " )"
  a :@: x -> "(" ++ (toTHFTerm a) ++ "@" ++ (toTHFTerm x) ++ ")"
  Forall x a -> "( ![" ++ (toTHFVarName x) ++ ": " ++ (toTHFType $ Var x) ++ "]: " ++ (toTHFTerm a) ++ " )"
  Exists x a -> "( ![" ++ (toTHFVarName x) ++ ": " ++ (toTHFType $ Var x) ++ "]: " ++ (toTHFTerm a) ++ " )" -- TODO: figure out what not is
  Def a -> name $ Def a

toTPTP :: [SomeHOLDef] -> [String]
toTPTP defs = concat $ map (\(SomeHOLDef a) -> toTHF a) defs

valid :: [SomeHOLDef] -> Bool
valid _ = True
\end{code}