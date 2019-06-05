\section{THF AST}

\subsection{Usage}

\begin{code}
module Logic.TPTP.THF.AST where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Foldable (foldr, toList)
import Data.Monoid ((<>))

data THFType
  = THFBaseType String
  | THFVarType String
  | THFFuncType THFType THFType deriving (Show, Eq, Ord)

data THFVar u
  = THFVar u THFType String deriving (Show, Eq, Ord)

data THFConst u
  = THFConst u THFType String
  | THFDef u THFType String (THFTerm u) deriving (Show, Eq, Ord)

data THFTerm u
  = T u
  | F u
  | Var    u (THFVar u)
  | Const  u (THFConst u)
  | Not    u (THFTerm u)
  | And    u (THFTerm u) (THFTerm u)
  | Or     u (THFTerm u) (THFTerm u)
  | Imply  u (THFTerm u) (THFTerm u)
  | App    u (THFTerm u) (THFTerm u)
  | Lam    u [THFVar u] (THFTerm u)
  | Forall u [THFVar u] (THFTerm u)
  | Exists u [THFVar u] (THFTerm u)
  | Equal  u (THFTerm u) (THFTerm u) deriving (Show, Eq, Ord)

instance Functor THFVar where
  fmap f x = case x of
    THFVar u typeRep name -> THFVar (f u) typeRep name

instance Functor THFConst where
  fmap f x = case x of
    THFConst u typeRep name -> THFConst (f u) typeRep name
    THFDef u typeRep name definition -> THFDef (f u) typeRep name (fmap f definition)

instance Functor THFTerm where
  fmap f x = case x of 
    T u -> T (f u)
    F u -> F (f u)
    Var u v       -> Var    (f u) (fmap f v)
    Const u c     -> Const  (f u) (fmap f c)
    Not u t       -> Not    (f u) (fmap f t)
    And u t s     -> And    (f u) (fmap f t) (fmap f s)
    Or u t s      -> Or     (f u) (fmap f t) (fmap f s)
    Imply u t s   -> Imply  (f u) (fmap f t) (fmap f s)
    App u t s     -> App    (f u) (fmap f t) (fmap f s)
    Lam u vs t    -> Lam    (f u) (fmap (fmap f) vs) (fmap f t)
    Forall u vs t -> Forall (f u) (fmap (fmap f) vs) (fmap f t)
    Exists u vs t -> Exists (f u) (fmap (fmap f) vs) (fmap f t)
    Equal u t s   -> Equal  (f u) (fmap f t) (fmap f s)

instance Foldable THFVar where
  -- foldMap :: Monoid m => (a -> m) -> THFVar a -> m
  -- foldr :: (a -> b -> b) -> b -> THFVar a -> b
  foldMap f (THFVar u _ _) = f u
  foldr g e (THFVar u _ _) = g u e

instance Traversable THFVar where
  -- traverse :: Applicative f => (a -> f b) -> THFVar a -> f (THFVar b)
  traverse f (THFVar u typeRep name) = (\v -> THFVar v typeRep name) <$> f u

instance Foldable THFConst where
  -- foldMap :: Monoid m => (a -> m) -> THFConst a -> m
  -- foldr :: (a -> b -> b) -> b -> THFConst a -> b
  foldMap f x = case x of
    (THFConst u _ _) -> f u
    (THFDef u _ _ t) -> f u <> foldMap f t

  foldr g e x = case x of
    (THFConst u _ _) -> g u e
    (THFDef u _ _ t) -> g u $ foldr g e t

instance Traversable THFConst where
  -- traverse :: Applicative f => (a -> f b) -> THFConst a -> f (THFConst b)
  traverse f x = case x of
    (THFConst u typeRep name)          -> THFConst <$> f u <*> pure typeRep <*> pure name
    (THFDef u typeRep name definition) -> THFDef <$> f u <*> pure typeRep <*> pure name <*> traverse f definition

instance Foldable THFTerm where
  -- foldMap :: Monoid m => (a -> m) -> THFTerm a -> m
  -- foldr :: (a -> b -> b) -> b -> THFTerm a -> b
  foldMap f x = case x of
    T u -> f u
    F u -> f u
    Var u v       -> f u <> foldMap f v
    Const u c     -> f u <> foldMap f c
    Not u t       -> f u <> foldMap f t
    And u t s     -> f u <> foldMap f t <> foldMap f s
    Or u t s      -> f u <> foldMap f t <> foldMap f s
    Imply u t s   -> f u <> foldMap f t <> foldMap f s
    App u t s     -> f u <> foldMap f t <> foldMap f s
    Lam u vs t    -> f u <> foldMap (foldMap f) vs <> foldMap f t
    Forall u vs t -> f u <> foldMap (foldMap f) vs <> foldMap f t
    Exists u vs t -> f u <> foldMap (foldMap f) vs <> foldMap f t
    Equal u t s   -> f u <> foldMap f t <> foldMap f s
  foldr g e x = case x of
    T u -> g u e
    F u -> g u e
    Var u v       -> g u $ foldr g e v
    Const u c     -> g u $ foldr g e c
    Not u t       -> g u $ foldr g e t
    And u t s     -> g u $ foldr g (foldr g e s) t 
    Or u t s      -> g u $ foldr g (foldr g e s) t 
    Imply u t s   -> g u $ foldr g (foldr g e s) t 
    App u t s     -> g u $ foldr g (foldr g e s) t 
    Lam u vs t    -> g u $ foldr g (foldr g e t) $ (concat . map toList) vs 
    Forall u vs t -> g u $ foldr g (foldr g e t) $ (concat . map toList) vs 
    Exists u vs t -> g u $ foldr g (foldr g e t) $ (concat . map toList) vs 
    Equal u t s   -> g u $ foldr g (foldr g e s) t 

instance Traversable THFTerm where
  -- traverse :: Applicative f => (a -> f b) -> THFTerm a -> f (THFTerm b)
  traverse f x = case x of
    T u -> T <$> f u
    F u -> F <$> f u
    Var u v       -> Var <$> f u <*> traverse f v
    Const u c     -> Const <$> f u <*> traverse f c
    Not u t       -> Not <$> f u <*> traverse f t
    And u t s     -> And <$> f u <*> traverse f t <*> traverse f s
    Or u t s      -> Or <$> f u <*> traverse f t <*> traverse f s
    Imply u t s   -> Imply <$> f u <*> traverse f t <*> traverse f s
    App u t s     -> App <$> f u <*> traverse f t <*> traverse f s
    Lam u vs t    -> Lam <$> f u <*> traverse (traverse f) vs <*> traverse f t
    Forall u vs t -> Forall <$> f u <*> traverse (traverse f) vs <*> traverse f t
    Exists u vs t -> Exists <$> f u <*> traverse (traverse f) vs <*> traverse f t
    Equal u t s   -> Equal <$> f u <*> traverse f t <*> traverse f s

insertVar :: (THFVar u) -> Map String (THFVar u) -> Map String (THFVar u)
insertConst :: (THFConst u) -> Map String (THFConst u) -> Map String (THFConst u)

insertVar v@(THFVar _ _ name) = Map.insert name v
insertConst c@(THFConst _ _ name) = Map.insert name c

mkVarMap :: [THFVar u] -> Map String (THFVar u)
mkConstMap :: [THFConst u] -> Map String (THFConst u)

mkVarMap [] = Map.empty
mkVarMap (x:xs) = insertVar x (mkVarMap xs)

mkConstMap [] = Map.empty
mkConstMap (x:xs) = insertConst x (mkConstMap xs)
\end{code}

\begin{code}
data AbstractTHFType 
  = AbstractTHFType String String deriving (Show, Eq, Ord)

data AbstractTHFFormulae
  = AbstractTHFAxiom String String
  | AbstractTHFDefinition String String
  | AbstractTHFConjecture String String deriving (Show, Eq, Ord)

data AbstractTHFFile =
  AbstractTHFFile [AbstractTHFType] [AbstractTHFFormulae] deriving (Show, Eq, Ord)
\end{code}
