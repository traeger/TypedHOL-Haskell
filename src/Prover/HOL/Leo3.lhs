\begin{code}
{-# LANGUAGE TypeFamilies #-}

module Prover.HOL.Leo3
  ( valid
  ) where

import HOL
import TPTP_THF

import Prover
import SZSStatus (SZSStatus)
import qualified SZSStatus as SZSStatus

import System.Process
import Data.List (isPrefixOf)
import Data.Maybe (fromJust)
import Data.Typeable

leo3SZSStatusPrefix = "% SZS status "

valid' :: Typeable u => [SomeHOLConst u] -> HOLTerm Bool u -> IO SZSStatus
valid' csts conj = do
  let tptp = toTPTP csts conj
  result <- runProver tptp
  return $ fromJust $ SZSStatus.fromName $ head $ words $ drop (length leo3SZSStatusPrefix) result

runProver :: [String] -> IO String
runProver tptp = do
  out <- readProcess "leo3" ["/dev/stdin", "-v", "0"] (unlines tptp)

  putStrLn out

  let status = filter (isPrefixOf leo3SZSStatusPrefix) $ lines out
  return $ head status

data HOLProver_Leo3 u = HOLProver_Leo3
instance Typeable u => Prover (HOLProver_Leo3 u) where
  type ProveConstant (HOLProver_Leo3 u) = SomeHOLConst u
  type ProveConjecture (HOLProver_Leo3 u) = HOLTerm Bool u

  validP HOLProver_Leo3 csts conj = valid' csts conj

valid :: Typeable u => [SomeHOLConst u] -> HOLTerm Bool u -> IO SZSStatus
valid = validP HOLProver_Leo3
\end{code}