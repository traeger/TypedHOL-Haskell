Experimental, not working at all.

\begin{code}
module Logic.TPTP.ParserCore where

import Control.Monad (void)
import Control.Monad.Combinators.Expr -- from parser-combinators
import Data.Void
import Text.Megaparsec hiding (parse, parseTest)
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Set as Set

type Parser = Parsec Void String
type ParserT = ParsecT Void String

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "%"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

identifier :: Parser String
identifier = (:) <$> letterChar <*> many alphaNumChar

rword :: String -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- to enforce a parse error with the text "hallo" use
-- do
--   textError "hallo"
textError :: String -> ParserT s a
textError err = fancyFailure (Set.fromList $ [ErrorFail $ show $ err])

parseJust :: Show a => Parser a -> String -> a
parseJust parser s = 
  case MP.runParser (between sc eof parser) "" s of
    Right a -> a
    Left e  -> error $ errorBundlePretty e

parseTest :: Show a => Parser a -> String -> IO ()
parseTest parser = MP.parseTest (between sc eof parser)
\end{code}
MP.runParser
  :: Parsec e s a -- ^ Parser to run
  -> String     -- ^ Name of source file
  -> s          -- ^ Input for parser
  -> Either (ParseErrorBundle s e) a
