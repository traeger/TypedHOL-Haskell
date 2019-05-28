Experimental, not working at all.

\begin{code}
{-# LANGUAGE GADTs, KindSignatures, ScopedTypeVariables, ConstraintKinds, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

module TPTP_Parser where

import HOL

import Data.Typeable

import Control.Monad (void)
import Control.Monad.Combinators.Expr -- from parser-combinators
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "%"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

identifier :: Parser String
identifier = (:) <$> letterChar <*> many alphaNumChar

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
\end{code}
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