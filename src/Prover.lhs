\begin{code}
{-# LANGUAGE TypeFamilies #-}

module Prover where

import SZSStatus (SZSStatus)
import qualified SZSStatus as SZSStatus

class Prover p where
  type ProveConstant p
  type ProveConjecture p

  validP :: p -> [ProveConstant p] -> ProveConjecture p -> IO SZSStatus
\end{code}