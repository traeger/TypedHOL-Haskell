\begin{code}
{-# LANGUAGE TypeFamilies #-}

module Logic.Prover
( SZSStatus(..), SZSSuccessReason(..), SZSNoSuccessReason(..)
, Prover(..)
, szsStatusFromName, szsStatusFromType
) where

import Logic.Prover.SZSStatus

class Prover p where
  type ProveFormulae p
  type ProveConjecture p

  validP :: p -> [ProveFormulae p] -> ProveConjecture p -> IO SZSStatus
\end{code}