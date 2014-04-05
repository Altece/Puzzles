module Trie where

 import qualified Data.Map as Map

 data Trie x = Empty | Node (Maybe x) (Map.Map Char (Trie x)) deriving Show

 empty :: Trie x
 empty = Empty

 insert :: String -> x -> Trie x -> Trie x
 insert cs     x  Empty = insert cs x $ Node Nothing Map.empty
 insert []     x' (Node x m) = Node (Just x') m
 insert (c:cs) x' (Node x m) = 
   case Map.lookup c m of
     Nothing -> Node x (Map.insert c (insert cs x' Empty) m)
     Just t -> Node x (Map.insert c (insert cs x' t) m)

{-
 {- This remove would cause lookup to be inefficient -}
 remove :: Trie x -> String -> Trie x
 remove Empty _ = Empty
 remove (Node x m) [] = Node Nothing m
 remove (Node x m) (c:cs) = 
   case Map.lookup c m of
     Nothing -> Node x m
     Just t -> Node x (Map.insert c (remove t cs))
-}

 lookup :: String -> Trie x -> Maybe x
 lookup cs Empty = Nothing
 lookup cs (Node x m) =
   case cs of
     [] -> x
     (c:cs) -> Map.lookup c m >>= \t -> Trie.lookup cs t

 wildcard = '*'

 findAll :: String -> Trie x -> [(String, x)]
 findAll _  Empty = []
 findAll cs (Node x m) = foldr checkAll [] combinations
   where remove c [] = []
         remove c (x:xs) = if c == x then xs else x:(remove c xs)
         combinations = foldr dealWithWildcard [] $ map (\c -> (c, remove c cs)) cs
           where dealWithWildcard (c, rest) l =
                   case c of
                     wildcard -> (map (\c -> (c, rest)) letters) ++ l
                     _ -> (c, rest):l
                 letters = ['a'..'z'] ++ ['A'..'Z']
         checkAll (c, rest) l = 
           case Map.lookup c m of
             Nothing -> l
             Just t -> 
               case t of
                 Empty -> l
                 Node Nothing m -> 
                   (l ++) $ map (\(cs, x) -> (c:cs, x)) $ findAll rest t
                 Node (Just x) m -> 
                   ((([c], x):l) ++) $ map (\(cs, x) -> (c:cs, x)) $ findAll rest t
