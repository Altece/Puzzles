module Trie (
  Trie,
  empty,
  insert,
  isNode,
  isEmpty,
  Trie.lookup,
  lookup',
  contains,
  subtrie,
  subtrie',
  Trie.find,
  find'
) where

 import Data.Foldable
 import qualified Data.Map as Map
 import Data.Maybe
 import qualified Data.List as List
 import Data.Traversable

 data Trie k v = Empty
               | Node v (Trie k v)
               | Root (Map.Map k (Trie k v))
 
 instance (Eq k, Eq v) => Eq (Trie k v) where
   (==) Empty Empty = True
   (==) (Root lm) (Root rm) = lm == rm
   (==) (Node l lm) (Node r rm) = l == r && lm == rm
   (==) _ _ = False
 
 instance Functor (Trie k) where
   fmap f Empty = Empty
   fmap f (Node x t) = Node (f x) . fmap f $ t
   fmap f (Root m) = Root . flip fmap m . fmap $ f
   
 instance Foldable (Trie k) where
   foldr f z Empty = z
   foldr f z (Node x t) = foldr f (f x z) t
   foldr f z (Root m) = Map.foldr (flip $ foldr f) z m
   
 instance Traversable (Trie k) where
   traverse f Empty = pure Empty
   traverse f (Node x t) = Node <$> f x <*> traverse f t
   traverse f (Root m) = Root <$> traverse (traverse f) m

 empty :: Trie k v
 empty = Empty
 
 insert :: Ord k => [k] -> v -> Trie k v -> Trie k v
 insert [] v Empty = Node v Empty
 insert [] v (Node _ t) = Node v t
 insert [] v (Root m) = Root m
 insert ks v Empty = insert ks v . Root $ Map.empty
 insert ks v (Node x t) = Node x . insert ks v $ t
 insert (k:ks) v (Root m) = Root . flip (Map.insert k) m
                                 . insert ks v
                                 . maybe Empty id 
                                 . Map.lookup k $ m
 
 isNode :: Trie k a -> Bool
 isNode (Node _ _) = True
 isNode _ = False
 
 isEmpty :: Trie k a -> Bool
 isEmpty Empty = True
 isEmpty (Node _ _) = False
 isEmpty (Root ts) = Map.size ts /= 0
 
 lookup' :: Ord k => (k -> Bool) -> [k] -> Trie k a -> [([k], a)]
 lookup' _ [] (Node a _) = [([], a)]
 lookup' _ [] _          = []
 lookup' _ _  Empty      = []
 lookup' p (k:ks) (Node a t) = ([k], a) : lookup' p ks t
 lookup' p (k:ks) t@(Root ts)
   | p k  = concatMap (flip (lookup' p) t) . map (:ks) . Map.keys $ ts
   | True = fromMaybe [] $ do st <- Map.lookup k ts
                              let sts = lookup' p ks st
                              return . flip map sts . mapFst $ (k:)
 
 lookup :: Ord k => [k] -> Trie k a -> Maybe a
 lookup [] (Node a _) = Just a
 lookup ks t = subtrie ks t >>= Trie.lookup []
 
 contains :: Ord k => [k] -> Trie k v -> Bool
 contains ks t = isJust . Trie.lookup ks $ t

 subtrie' :: Ord k => (k -> Bool) -> [k] -> Trie k a -> [Trie k a]
 subtrie' p (k:ks) Empty      = []
 subtrie' p (k:ks) (Node _ _) = []
 subtrie' p []     Empty      = []
 subtrie' p []     t          = [t]
 subtrie' p (k:ks) t@(Root ts)
   | p k  = concatMap (flip (subtrie' p) t) $ map (:ks) . Map.keys $ ts
   | True = fromMaybe [] $ do sts <- Map.lookup k ts
                              return . subtrie' p ks $ sts

 subtrie :: Ord k => [k] -> Trie k a -> Maybe (Trie k a)
 subtrie ks t = listToMaybe . subtrie' (const False) ks $ t
 
 -- This is not optimal, but it's "good enough" for now.
 find' :: Ord k => (k -> Bool) -> [k] -> Trie k a -> [([k], a)]
 find' p ks t = concatMap (\ks -> lookup' p ks t) $ List.permutations ks
 
 find = find' (const False)
 
 mapFst :: (a -> b) -> (a, c) -> (b, c)
 mapFst f (x, y) = (f x, y)
