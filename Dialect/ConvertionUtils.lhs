\begin{code}
module Logic.Dialect.ConversionUtils where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap

newtype SymbolMap a = SymbolMap (Bimap a Int) deriving (Show, Eq, Ord)
mkSymbolMap :: Ord a => Set a -> SymbolMap a
mkSymbolMap = SymbolMap . intMapping where
  intMapping :: Ord a => Set a -> Bimap a Int
  intMapping = Bimap.fromList . (flip zip) [0..] . Set.elems

toSymbol :: Ord a => SymbolMap a -> a -> String
toSymbol (SymbolMap m) a = '_' : (show $ m Bimap.! a)

fromSymbol :: Ord a => SymbolMap a -> String -> Maybe a
fromSymbol (SymbolMap m) ('_':xs) = Just $ m Bimap.!> (read xs)
fromSymbol _ _ = Nothing
\end{code}
