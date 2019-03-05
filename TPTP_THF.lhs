\begin{code}
{-# LANGUAGE ViewPatterns, TypeFamilies, RankNTypes #-}

module TPTP_THF where

import HOL

import Data.Typeable
import qualified Data.Char as Char

import Prelude hiding (forall, exists)

toTHF :: Typeable t => HOLDef t -> [String]
toTHF (HOLDefTerm name term) = 
  ["thf(" ++ name ++ "_decl,type,(" ++ name ++ ":" ++ (toTHFType term) ++ "))."
  ,"thf(" ++ name ++ ",definition,(" ++ name ++ "=(" ++ (toTHFTerm term) ++ ")))."]
toTHF (HOLDefConst cst) = 
  ["thf(" ++ (toTHFConstName cst) ++ ",type,(" ++ (toTHFConstName cst) ++ ":" ++ (toTHFType $ Const cst) ++ "))."]

toTHFConjecture :: HOLTerm Bool -> [String]
toTHFConjecture term = 
  ["thf(conj,conjecture,(conj=(" ++ (toTHFTerm term) ++ ")))."]

toTHFType :: Typeable t => HOLTerm t -> String
toTHFType term = toTHFType' $ getHOLType term where
  toTHFType' x = case splitTyConApp x of
    (_,[y,z]) -> (toTHFType' y) ++ " > " ++ (toTHFType' z) -- TODO: add (->) to pattern match, but how?
    (show -> "Bool", [])   -> "$o"
    (_, [])   -> "$i"

-- TPTP vars need to be upper case
toTHFVarName :: HOLVar t -> String
toTHFVarName (HOLVar name) = case name of
  (x:xs) -> (Char.toUpper x):xs

-- TPTP constant need to be lower case
toTHFConstName :: HOLConst t -> String
toTHFConstName (HOLConst name) = case name of
  (x:xs) -> (Char.toLower x):xs

--  toTHFType' (Fun a b) = (toTHFType' a) ++ " > " ++ (toTHFType' b)

toTHFTerm :: Typeable t => HOLTerm t -> String
toTHFTerm term = case term of
  T -> "$true"
  F -> "$false"
  Var v -> toTHFVarName v
  Const c -> toTHFConstName c
  Not a -> "" ++ (toTHFTerm a) -- TODO: figure out what not is
  a :&: b -> "(" ++ (toTHFTerm a) ++ "&" ++ (toTHFTerm b) ++ ")"
  a :|: b -> "(" ++ (toTHFTerm a) ++ "|" ++ (toTHFTerm b) ++ ")"
  a :->: b -> "(" ++ (toTHFTerm a) ++ "=>" ++ (toTHFTerm b) ++ ")"
  Lam x a -> "( ^[" ++ (toTHFVarName x) ++ ": " ++ (toTHFType $ Var x) ++ "]: " ++ (toTHFTerm a) ++ " )"
  a :@: x -> "(" ++ (toTHFTerm a) ++ "@" ++ (toTHFTerm x) ++ ")"
  Forall x a -> "( ![" ++ (toTHFVarName x) ++ ": " ++ (toTHFType $ Var x) ++ "]: " ++ (toTHFTerm a) ++ " )"
  Exists x a -> "( ![" ++ (toTHFVarName x) ++ ": " ++ (toTHFType $ Var x) ++ "]: " ++ (toTHFTerm a) ++ " )" -- TODO: figure out what not is

toTPTP :: [SomeHOLDef] -> HOLTerm Bool -> [String]
toTPTP defs conj = tptp_defs ++ tptp_conj where
  tptp_conj = toTHFConjecture conj
  tptp_defs = concat $ map (\(SomeHOLDef a) -> toTHF a) defs

valid :: [SomeHOLDef] -> Bool
valid _ = True
\end{code}