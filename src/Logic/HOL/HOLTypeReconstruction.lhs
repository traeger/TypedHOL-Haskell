\begin{code}
module Logic.HOL.HOLTypeReconstruction where

constantGen :: Typeable u => TypeRep -> String -> SomeHOLConst u
constantGen typeRep name = case typeRep of
  (show -> "Bool") -> gen (constant name :: Typeable u => HOLConst u (Bool))
  (show -> "Int") -> gen (constant name :: Typeable u => HOLConst u (Int))
  (show -> "Bool -> Bool") -> gen (constant name :: Typeable u => HOLConst u (Bool -> Bool))
  (show -> "Int -> Bool") -> gen (constant name :: Typeable u => HOLConst u (Int -> Bool))
  (show -> "Bool -> Int") -> gen (constant name :: Typeable u => HOLConst u (Bool -> Int))
\end{code}
