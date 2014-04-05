module Cipher where

 import Data.Char
 import qualified Data.Map as Map

 type CipherText = String
 type PlainText = String

 toInt :: Char -> Int
 toInt c = if isAlpha c then (ord $ toUpper c) - (ord 'A')
                        else error "toInt will only work for alpha chars"

 toChar :: Int -> Char
 toChar i = chr $ i + (ord 'A')

 class Cipher c where
   encode :: PlainText -> c -> CipherText
   decode :: CipherText -> c -> PlainText

 data Caesar = Rot Int

 instance Cipher Caesar where
 
   encode []     _ = []
   encode (c:cs) (Rot x) = 
     if isAlpha c then (toChar $ (toInt c + x) `mod` 26):(encode cs $ Rot x)
                  else c:(encode cs $ Rot x)

   decode []     _ = []
   decode (c:cs) (Rot x) = 
     if isAlpha c then (toChar $ (toInt c - x) `mod` 26):(decode cs $ Rot x)
                  else c:(decode cs $ Rot x)

 newtype Morse = Morse [(Char, String)]

 morse :: Morse
 morse = Morse 
         [('A', ".-"),
          ('B', "-..."),
          ('C', "-.-."),
          ('D', "-.."),
          ('E', "."),
          ('F', "..-."),
          ('G', "--."),
          ('H', "...."),
          ('I', ".."),
          ('J', ".---"),
          ('K', "-.-"),
          ('L', ".-.."),
          ('M', "--"),
          ('N', "-."),
          ('O', "---"),
          ('P', ".--."),
          ('Q', "--.-"),
          ('R', ".-."),
          ('S', "..."),
          ('T', "-"),
          ('U', "..-"),
          ('V', "...-"),
          ('W', ".--"),
          ('X', "-..-"),
          ('Y', "-.--"),
          ('Z', "--.."),
          ('0', "-----"),
          ('1', ".----"),
          ('2', "..---"),
          ('3', "...--"),
          ('4', "....-"),
          ('5', "....."),
          ('6', "-...."),
          ('7', "--..."),
          ('8', "---.."),
          ('9', "----.")]

 splitOn :: (x -> Bool) -> [x] -> [[x]]
 splitOn f [] = []
 splitOn f xs = case first xs of (x, xs) -> x:(splitOn f xs)
   where first [] = ([], [])
         first (x:xs) = if f x then ([], xs) 
                               else case first xs of
                                      (xs', rest) -> (x:xs', rest)

 instance Cipher Morse where
 
   encode [] _ = []
   encode (c:cs) m = case lookup (toUpper c) m of
                       Nothing -> recurse
                       Just m -> if length recurse == 0 then m else m ++ " " ++ recurse
     where lookup c (Morse m) = foldl (\r (a, m) -> if c == a then Just m else r) Nothing m
           recurse = encode cs m

   decode [] _ = []
   decode c m = foldr convert [] $ splitOn (\x -> x == ' ') c
     where lookup c (Morse m) = foldl (\r (a, m) -> if c == m then Just a else r) Nothing m
           convert c rest = case lookup c m of
                              Nothing -> rest
                              Just a -> a:rest

 type Braille = Morse

 braille :: Braille
 braille = Morse 
           [('A', ".-----"),
            ('B', ".-.---"),
            ('C', "..----"),
            ('D', "..-.--"),
            ('E', ".--.--"),
            ('F', "...---"),
            ('G', "....--"),
            ('H', ".-..--"),
            ('I', "-.-..."),
            ('J', "-.--.."),
            ('K', ".---.-"),
            ('L', ".-.-.-"),
            ('M', "..--.-"),
            ('N', "..-..-"),
            ('O', ".--..-"),
            ('P', "...-.-"),
            ('Q', ".....-"),
            ('R', ".-...-"),
            ('S', "-..-.-"),
            ('T', "-....-"),
            ('U', ".---.."),
            ('V', ".-.-.."),
            ('W', "-...-."),
            ('X', "..--.."),
            ('Y', "..-..."),
            ('Z', ".--...")]
