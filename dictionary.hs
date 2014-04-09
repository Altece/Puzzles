module Dictionary where

 import Trie
 import Data.Char
 import Data.List
 import qualified Data.Map as Map

 type Dictionary = Trie String

 dictionary :: [String] -> Dictionary
 dictionary [] = empty
 dictionary (w:ws) = Trie.insert (map toUpper w) w $ dictionary ws

 readDictionary :: FilePath -> IO Dictionary
 readDictionary filename = do file <- readFile filename
                              return $ dictionary $ lines file

 find :: String -> Dictionary -> Maybe String
 find s d = Trie.lookup (map toUpper s) d

 search = Dictionary.find

 unscramble :: String -> Dictionary -> [String]
 unscramble s d = lexical $ minimal $ map (\(s, s') -> s') $ findAll (map toUpper s) d

 anagrams :: String -> Dictionary -> [String]
 anagrams s d = lexical $ minimal $ filter (\s -> length s == expected) $ unscramble s d
   where expected = length s

 possible :: [[Char]] -> Dictionary -> [String]
 possible ws Empty = []
 possible ws (Node x m) =
   case ws of
     [] -> case x of { Nothing -> [] ; Just s -> [s] }
     ws -> concat $ map (\w -> figureout w (without w ws)) ws
   where without w [] = []
         without w (x:xs) = if w == x then xs else x:(without w xs)
         figureout w ws = foldl (\r c -> case Map.lookup (toUpper c) m of
                                           Nothing -> r
                                           Just d -> r ++ (possible ws d)) [] w

 lexical :: [String] -> [String]
 lexical ws = sortBy (\x y -> (map toUpper x) `compare` (map toUpper y)) ws 

 minimal :: [String] -> [String]
 minimal ws = foldr (\x seen -> if x `elem` seen then seen else x:seen) [] ws
