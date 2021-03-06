\begin{code}
{-# LANGUAGE ViewPatterns, TypeFamilies, RankNTypes #-}

module Logic.TPTP.THF where

import Logic.HOL

import Data.Typeable
import qualified Data.Char as Char

import Prelude hiding (not, forall, exists)

toTHF :: (Typeable t, Typeable u) => HOLConst u t -> [String]
toTHF cst = case cst of
  HOLDef name term ->
    ["thf(" ++ name ++ "_decl,type,(" ++ name ++ ":" ++ (toTHFType term) ++ "))."
    ,"thf(" ++ name ++ ",definition,(" ++ name ++ "=(" ++ (toTHFTerm term) ++ ")))."]
  HOLConst _ ->
    ["thf(" ++ (toTHFConstName cst) ++ ",type,(" ++ (toTHFConstName cst) ++ ":" ++ (toTHFType $ Const cst) ++ "))."]

toTHFConjecture :: Typeable u => HOLTerm u Bool -> [String]
toTHFConjecture term = 
  ["thf(conj,conjecture,(" ++ (toTHFTerm term) ++ "))."]
\end{code}

TODO: Lookup the TPTP syntax definition to use the explicit Haskell types in TPTP instead of "$o", "$i".
\begin{code}
toTHFType :: (Typeable t, Typeable u) => HOLTerm u t -> String
toTHFType term = toTHFType' $ getHOLType term where
  toTHFType' x = case splitTyConApp x of
    (_,[y,z]) -> (toTHFType' y) ++ " > " ++ (toTHFType' z) -- TODO: add (->) to pattern match, but how?
    (show -> "Bool", [])   -> "$o"
    (_, [])   -> "$i"

-- TPTP vars need to be upper case
toTHFVarName :: HOLVar u t -> String
toTHFVarName (HOLVar name) = case name of
  (x:xs) -> (Char.toUpper x):xs

-- TPTP constants need to be lower case
toTHFConstName :: HOLConst u t -> String
toTHFConstName (HOLConst name) = case name of
  (x:xs) -> (Char.toLower x):xs

\end{code}

TODO: Use a better ShowS implementation to remove unnessesary "()".
TODO: introduce TPTP choice? How to model this in Haskell?
\begin{code}
toTHFTerm :: (Typeable t, Typeable u) => HOLTerm u t -> String
toTHFTerm term = case term of
  T -> "$true"
  F -> "$false"
  Var v -> toTHFVarName v
  Const c -> toTHFConstName c
  Not a -> "" ++ (toTHFTerm a) -- TODO: figure out what not in TPTP is
  And a b -> "(" ++ (toTHFTerm a) ++ "&" ++ (toTHFTerm b) ++ ")"
  Or a b -> "(" ++ (toTHFTerm a) ++ "|" ++ (toTHFTerm b) ++ ")"
  Imply a b -> "(" ++ (toTHFTerm a) ++ "=>" ++ (toTHFTerm b) ++ ")"
  Lam x a -> "( ^[" ++ (toTHFVarName x) ++ ": " ++ (toTHFType $ Var x) ++ "]: " ++ (toTHFTerm a) ++ " )"
  App a x -> "(" ++ (toTHFTerm a) ++ "@" ++ (toTHFTerm x) ++ ")"
  Forall x a -> "( ![" ++ (toTHFVarName x) ++ ": " ++ (toTHFType $ Var x) ++ "]: " ++ (toTHFTerm a) ++ " )"
  Exists x a -> "( ?[" ++ (toTHFVarName x) ++ ": " ++ (toTHFType $ Var x) ++ "]: " ++ (toTHFTerm a) ++ " )"

toTPTP :: Typeable u => [SomeHOLConst u] -> HOLTerm u Bool -> [String]
toTPTP defs conj = tptp_defs ++ tptp_conj where
  tptp_conj = toTHFConjecture conj
  tptp_defs = concat $ map (\(SomeHOLConst a) -> toTHF a) defs
\end{code}