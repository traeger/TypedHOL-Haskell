\section{Parser for Constant Declerations}

\subsection{Usage}
\begin{terminal}
*> parseJust constExprGen0 "h: \$o"
h :: Bool
*> parseJust constExprGen0 "h: \$i"
h :: Int
*> parseJust constExprGen0 "h: \$o < \$o"
h :: Bool -> Bool
*> parseJust constExprGen0 "h: \$i < \$o"
h :: Int -> Bool
*> parseJust constExprGen0 "h: \$o < \$i"
h :: Bool -> Int

*> parseJust constExpr "h: \$o" :: HOLConst () (Bool)
h :: Bool
*> parseJust constExpr "h: \$o < \$o" :: HOLConst () (Bool -> Bool)
h :: Bool -> Bool

*> parseJust constExpr "h: \$o < \$o" :: HOLConst () Bool
*** Exception: 1:11:
  |
1 | h: \$o < \$o
  |           ^
"type missmatch"
\end{terminal}

\begin{code}
{-# LANGUAGE ViewPatterns, RankNTypes, TypeFamilies #-}

module Logic.TPTP.THF.ConstParser 
( constExpr
, constExpr0

, HOLConst(..)
, HOLTerm(..)
, SomeHOLConst(..)
) where

import Logic.HOL
import Data.Typeable
import qualified Type.Reflection as R

import Logic.TPTP.ParserCore
import Logic.TPTP.THF.TypeParser

import Control.Monad
import Text.Megaparsec
import Control.Monad.Combinators.Expr

\end{code}
Parse a THF constant declaration. Examples are:
\begin{itemize}
  \item $x: \$i$
  \item $b: \$o$
  \item $f: \$b < \$b$
  \item $g: \$b < \$i < \$b$
\end{itemize}

TODO generalize this implementation or find a better one
TODO replace this crazy (show -> ..) pattern

inspired by
\begin{code}% example
convert :: SomeTypeRep -> String -> Maybe Dynamic
convert (SomeTypeRep rep) s
  | Just HRefl <- eqTypeRep rep (typeRep @String) = Just $ toDynamic s 
\end{code}% example
@see https://stackoverflow.com/questions/46992740/how-to-specify-type-of-value-via-typerep/46993294#46993294

\begin{code}
constantGen :: Typeable u => TypeRep -> String -> SomeHOLConst u
constantGen typeRep name = case typeRep of
  (show -> "Bool") -> gen (constant name :: Typeable u => HOLConst u (Bool))
  (show -> "Int") -> gen (constant name :: Typeable u => HOLConst u (Int))
  (show -> "Bool -> Bool") -> gen (constant name :: Typeable u => HOLConst u (Bool -> Bool))
  (show -> "Int -> Bool") -> gen (constant name :: Typeable u => HOLConst u (Int -> Bool))
  (show -> "Bool -> Int") -> gen (constant name :: Typeable u => HOLConst u (Bool -> Int))
\end{code}

\begin{code}
constExpr :: (Typeable u) => Parser (SomeHOLConst u)
constExpr = do
  name <- identifier
  symbol ":"
  typeFound <- typeExpr
  let c = constantGen typeFound name

  return $ c

-- TODO add better error message
-- constExpr :: (Typeable t, Typeable u) => Parser (HOLConst u t)
-- constExpr = do
--   c <- constExprGen
--   case unGen c of
--     Just c' -> return c'
--     _       -> textError "type missmatch"

constExpr0 :: Parser (SomeHOLConst ())
constExpr0 = constExpr

\end{code}
old:

constExpr :: (Typeable t, Typeable u) => Parser (HOLConst u t)
constExpr = do
  name <- identifier
  symbol ":"
  typeFound <- typeExpr
  let c = constant name
  let typeExspected = getHOLType c

  when (typeFound /= typeExspected) $ holTypeError typeExspected typeFound

  return c
