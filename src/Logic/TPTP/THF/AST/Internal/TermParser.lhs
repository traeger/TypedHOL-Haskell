\section{Parser for Terms}

\subsection{Usage}

\begin{terminal}
\item *> parseJust (termExprRaw) "^ [X: \$o] : ! [Y : \$o]: not Y & \$false"
Lam [HOLVar (HOLBaseType "o") "X"] (Forall [HOLVar (HOLBaseType "o") "Y"] (And (Not (Var (HOLVar (HOLBaseType "o") "Y"))) F))
\begin{terminal}
\begin{code}
module Logic.TPTP.THF.AST.Internal.TermParser where

import Logic.TPTP.ParserCore
import Logic.TPTP.THF.AST.Internal
import Logic.TPTP.THF.AST.Internal.TypeParser
import Logic.TPTP.THF.AST.Internal.ConstParser
import Logic.TPTP.THF.AST.Internal.VarParser

import Control.Monad
import Text.Megaparsec
import Control.Monad.Combinators.Expr

import Data.Map (Map)
import Data.Map as Map
import Data.Monoid ((<>))

\end{code}

Parse a THF term. Examples are:
\begin{itemize}
\end{itemize}
\begin{code}
termExpr :: Map String HOLConst -> Parser HOLTerm
termExpr = (flip termExpr') Map.empty

termExpr' :: Map String HOLConst -> Map String HOLVar -> Parser HOLTerm
termExpr' consts vars = makeExprParser (termTerm consts vars) (termOperators consts vars)

termTerm :: Map String HOLConst -> Map String HOLVar -> Parser HOLTerm
termTerm consts vars = parens (termTerm consts vars)
  <|> T <$ rword "$true"
  <|> F <$ rword "$false" 
  <|> (termVarExpr vars)
  <|> (termConstExpr consts)
  <|> (termLamExpr consts vars)
  <|> (termForallExpr consts vars)
  <|> (termExistsExpr consts vars)

termVarExpr :: Map String HOLVar -> Parser HOLTerm
termVarExpr vars = do
  name <- varIdentifier
  case vars !? name of
    Just v  -> return $ Var v
    Nothing -> textError $ "variable " ++ name ++ " not defined"

termConstExpr :: Map String HOLConst -> Parser HOLTerm
termConstExpr consts = do
  name <- constIdentifier
  case consts !? name of
    Just c  -> return $ Const c
    Nothing -> textError $ "constant " ++ name ++ " not defined"

termLamExpr :: Map String HOLConst -> Map String HOLVar -> Parser HOLTerm
termLamExpr consts vars = do
  symbol "^"
  varsLam <- varsExpr
  symbol ":"
  termLam <- termExpr' consts (vars <> mkVarMap varsLam)
  return $ Lam varsLam termLam

termForallExpr :: Map String HOLConst -> Map String HOLVar -> Parser HOLTerm
termForallExpr consts vars = do
  symbol "!"
  varsLam <- varsExpr
  symbol ":"
  termLam <- termExpr' consts (vars <> mkVarMap varsLam)
  return $ Forall varsLam termLam

termExistsExpr :: Map String HOLConst -> Map String HOLVar -> Parser HOLTerm
termExistsExpr consts vars = do
  symbol "?"
  varsLam <- varsExpr
  symbol ":"
  termLam <- termExpr' consts (vars <> mkVarMap varsLam)
  return $ Exists varsLam termLam

-- Every inner list is a list of operators we want to support, they all have equal precedence. 
-- The outer list is ordered in descending precedence, so the higher we place a group of operators in it, the tighter they bind.
-- TODO fix the operator strength and associative type
termOperators :: Map String HOLConst -> Map String HOLVar -> [[Operator Parser HOLTerm]]
termOperators consts vars =
  [ [ InfixL $ Equal <$ symbol "=" ]
  , [ Prefix $ Not <$ symbol "not" ]
  , [ InfixL $ App <$ symbol "@" ]
  , [ InfixL $ And <$ symbol "&" ]
  , [ InfixL $ Imply <$ symbol "->" ]
  ]
\end{code}

