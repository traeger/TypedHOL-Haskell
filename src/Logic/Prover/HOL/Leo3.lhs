\begin{code}
{-# LANGUAGE TypeFamilies #-}

module Logic.Prover.HOL.Leo3
  ( valid
  , toTPTP
  ) where

import Logic.HOL
import Logic.TPTP.THF

import Logic.Prover

import System.Process
import Data.List (isPrefixOf)
import Data.Maybe (fromJust)
import Data.Typeable

leo3SZSStatusPrefix = "% SZS status "

valid' :: Typeable u => [SomeHOLFormulae u] -> HOLTerm Bool u -> IO SZSStatus
valid' csts conj = do
  let tptp = toTPTP csts conj
  result <- runProver tptp
  return $ fromJust $ szsStatusFromName $ head $ words $ drop (length leo3SZSStatusPrefix) result

runProver :: [String] -> IO String
runProver tptp = do
  out <- readProcess "leo3" ["/dev/stdin", "-v", "0"] (unlines tptp)

  putStrLn out

  let status = filter (isPrefixOf leo3SZSStatusPrefix) $ lines out
  return $ head status

-- right now, this abstraction makes no sence at all. But: hopefully provers will have generalized functionality someday :)
data HOLProver_Leo3 u = HOLProver_Leo3
instance Typeable u => Prover (HOLProver_Leo3 u) where
  type ProveFormulae (HOLProver_Leo3 u) = SomeHOLFormulae u
  type ProveConjecture (HOLProver_Leo3 u) = HOLTerm Bool u

  validP HOLProver_Leo3 csts conj = valid' csts conj

valid :: Typeable u => [SomeHOLFormulae u] -> HOLTerm Bool u -> IO SZSStatus
valid = validP HOLProver_Leo3
\end{code}