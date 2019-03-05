\begin{code}
module Leo3 where

import HOL
import TPTP_THF
import System.Process

import Data.List (isPrefixOf)

import Data.Typeable

valid :: Typeable u => [SomeHOLConst u] -> HOLTerm Bool u -> IO Bool
valid csts conj = do
  let tptp = toTPTP csts conj
  result <- runProver tptp
  return $ isPrefixOf "% SZS status Theorem" result

runProver :: [String] -> IO String
runProver tptp = do
  out <- readProcess "leo3" ["/dev/stdin", "-v", "0"] (unlines tptp)

  putStrLn out

  let status = filter (isPrefixOf "% SZS status") $ lines out
  return $ head status
\end{code}