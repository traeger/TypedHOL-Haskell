Experimental, not working at all.

\begin{code}
{-# LANGUAGE GADTs, KindSignatures, ScopedTypeVariables, ConstraintKinds, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

module Logic.TPTP.THF.TypeParser 
( typeExpr
) where

import Logic.HOL
import Data.Typeable

import Logic.TPTP.ParserCore

import Text.Megaparsec
import Control.Monad.Combinators.Expr

typeExpr :: Parser TypeRep
typeExpr = makeExprParser typeTerm typeOperators

typeTerm :: Parser TypeRep
typeTerm = parens typeExpr
  <|> (typeOf (undefined :: Bool)) <$ rword "$o"
  <|> (typeOf (undefined :: Int)) <$ rword "$i" 
  -- we use int for now, since the $i type is not used in any specific why 

typeOperators :: [[Operator Parser TypeRep]]
typeOperators =
  [ [ InfixL (mkFunTy <$ symbol "<") ]
  ]

\end{code}
parseType :: Parser TypeRep
parseType = parseFunc' <|> parseSimpleType' where
  parseBool' = do
    rword "$o"
    return $ typeOf (undefined :: Bool) 
  parseInvididual' = do
    rword "$i"
    return $ typeOf (undefined :: Int) -- we use int for now, since the $i type is not used in any specific why anyway
  parseSimpleType' = parseBool' <|> parseInvididual'
  parseFunc' = do
    x <- parseSimpleType'
    rword ">"
    y <- parseType
    return $ mkFunTy x y


class (Typeable t, Typeable u) => TypeParser a t u where
  parseType :: Parser (a t u)

instance Typeable u => TypeParser HOLVar Bool u where
  parseType = do 
    string "$o"
    return undefined
instance Typeable u => TypeParser HOLVar Int u where
  parseType = do 
    string "$i"
    return undefined
instance (Typeable u, TypeParser HOLVar s u, TypeParser HOLTerm t u) => TypeParser HOLTerm (s -> t) u where
  parseType = do
    x <- parseType
    string ">"
    f <- parseType
    return $ Lam x f

testparse parser str = parseTest (between sc eof parser) str
thf_type_bool :: Parser Bool
thf_type_bool = do
  string "$o"
  return undefined

thf_type_identifier :: Parser Int
thf_type_identifier = do
  string "$i"
  return undefined

thf_type = thf_type_bool <|> thf_type_identifier

thf_constant :: Typeable t => Parser (HOLConst t ())
thf_constant = do 
  string "thf("
  thf_name <- identifier
  string ","
  string "type"
  string ","
  constant_name <- identifier
  string ")."
  return $ HOLConst constant_name

xs :: Typeable t => Parser (HOLConst t ())
xs = between sc eof x