module Dictionary where

 import Trie
 import Data.Char
 import qualified Data.Map as Map

 type Dictionary = Trie String

 dictionary :: [String] -> Dictionary
 dictionary [] = empty
 dictionary (c:cs) = insert (map toUpper c) c $ dictionary cs

 readDictionary :: FilePath -> IO Dictionary
 readDictionary filename = do file <- readFile filename
                              return $ dictionary $ lines file

 find :: String -> Dictionary -> Maybe String
 find s d = Trie.lookup (map toUpper s) d

 findWhere :: String -> Dictionary -> [String]
 findWhere _      Empty = []
 findWhere []     (Node x m) = case x of { Nothing -> []; Just x -> [x] }
 findWhere (c:cs) (Node x m) = 
   case c of
     wildcard -> foldl (++) [] $ map (\c -> findWhere (c:cs) $ Node x m) letters
     _ -> case x of { Nothing -> recurse ; Just x -> x:recurse }
   where recurse = case Map.lookup c m of
                     Nothing -> []
                     Just Empty -> []
                     Just d -> findWhere cs d
         letters = ['A'..'Z']

 unscramble :: String -> Dictionary -> [String]
 unscramble s d = map (\(s, s') -> s') $ findAll (map toUpper s) d

 anagrams :: String -> Dictionary -> [String]
 anagrams s d = filter (\s -> length s == expected) $ unscramble s d
   where expected = length s
