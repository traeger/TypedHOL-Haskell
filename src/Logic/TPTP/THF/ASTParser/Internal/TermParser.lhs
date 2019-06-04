\section{Parser for Terms}

\subsection{Usage}

\begin{terminal}
\item *> parseJust (termParserRaw) "^ [X: \$o] : ! [Y : \$o]: not Y & \$false"
Lam [HOLVar (HOLBaseType "o") "X"] (Forall [HOLVar (HOLBaseType "o") "Y"] (And (Not (Var (HOLVar (HOLBaseType "o") "Y"))) F))
\begin{terminal}
\begin{code}
module Logic.TPTP.THF.ASTParser.Internal.TermParser where

import Logic.TPTP.ParserCore
import Logic.TPTP.THF.AST
import Logic.TPTP.THF.ASTParser.Internal.TypeParser
import Logic.TPTP.THF.ASTParser.Internal.ConstParser
import Logic.TPTP.THF.ASTParser.Internal.VarParser

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
termParser :: Map String HOLConst -> Parser HOLTerm
termParser = (flip termParser') Map.empty

termParser' :: Map String HOLConst -> Map String HOLVar -> Parser HOLTerm
termParser' consts vars = makeExprParser (termTerm consts vars) (termOperators consts vars)

termTerm :: Map String HOLConst -> Map String HOLVar -> Parser HOLTerm
termTerm consts vars = parens (termTerm consts vars)
  <|> T <$ rword "$true"
  <|> F <$ rword "$false" 
  <|> (termVarParser vars)
  <|> (termConstParser consts)
  <|> (termLamParser consts vars)
  <|> (termForallParser consts vars)
  <|> (termExistsParser consts vars)

termVarParser :: Map String HOLVar -> Parser HOLTerm
termVarParser vars = do
  name <- varIdentifier
  case vars !? name of
    Just v  -> return $ Var v
    Nothing -> textError $ "variable " ++ name ++ " not defined"

termConstParser :: Map String HOLConst -> Parser HOLTerm
termConstParser consts = do
  name <- constIdentifier
  case consts !? name of
    Just c  -> return $ Const c
    Nothing -> textError $ "constant " ++ name ++ " not defined"

termLamParser :: Map String HOLConst -> Map String HOLVar -> Parser HOLTerm
termLamParser consts vars = do
  symbol "^"
  varsLam <- varsParser
  symbol ":"
  termLam <- termParser' consts (vars <> mkVarMap varsLam)
  return $ Lam varsLam termLam

termForallParser :: Map String HOLConst -> Map String HOLVar -> Parser HOLTerm
termForallParser consts vars = do
  symbol "!"
  varsLam <- varsParser
  symbol ":"
  termLam <- termParser' consts (vars <> mkVarMap varsLam)
  return $ Forall varsLam termLam

termExistsParser :: Map String HOLConst -> Map String HOLVar -> Parser HOLTerm
termExistsParser consts vars = do
  symbol "?"
  varsLam <- varsParser
  symbol ":"
  termLam <- termParser' consts (vars <> mkVarMap varsLam)
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

