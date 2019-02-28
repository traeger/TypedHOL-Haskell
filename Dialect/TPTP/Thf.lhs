\begin{code}
module Logic.TPTP.Thf where

import Logic.HOL

class ToThf t where
  toThf :: t -> String
class ParseThf t where
  parseThf :: String -> Maybe t

instance ToThf HOLType where
  toThf O = "$o"
  toThf I = "$i"
  toThf (O :=>: t) = "$o > " ++ (toThf t)
  toThf (I :=>: t) = "$i > " ++ (toThf t)
  toThf (s :=>: t) = "(" ++ (toThf s) ++ ") > " ++ (toThf t)

instance ToThf (HOLVar u) where
  toThf (SV t v) = "V" ++ v
  toThf (BV t i) = "Q" ++ show i

instance ToThf u => ToThf (HOLTerm u) where
  toThf p = case p of
    T           -> "$true"
    F           -> "$false"
    C t u       -> "c" ++ (toThf u)
    V v         -> toThf v
    Not q       -> "~" ++ (toThf q)
    q0 :&: q1   -> "(" ++ (toThf q0) ++ "&" ++ (toThf q1) ++ ")"
    q0 :|: q1   -> "(" ++ (toThf q0) ++ "|" ++ (toThf q1) ++ ")"
    q0 :-->: q1 -> "(" ++ (toThf q0) ++ "=>" ++ (toThf q1) ++ ")"
    q0 :<->: q1 -> "(" ++ (toThf q0) ++ "<=>" ++ (toThf q1) ++ ")"
    q0 :=: q1   -> "(" ++ (toThf q0) ++ "=" ++ (toThf q1) ++ ")"
    Forall v q  -> "![" ++ (toThf v) ++ ":" ++ (toThf $ holType v) ++ "] : (" ++ (toThf q) ++ ")"
    Exists v q  -> "?[" ++ (toThf v) ++ ":" ++ (toThf $ holType v) ++ "] : (" ++ (toThf q) ++ ")"
    v :/: q     -> "^[" ++ (toThf v) ++ ":" ++ (toThf $ holType v) ++ "] : (" ++ (toThf q) ++ ")"
    q0 :@: q1   -> "(" ++ (toThf q0) ++ "@" ++ (toThf q1) ++ ")"
\end{code}