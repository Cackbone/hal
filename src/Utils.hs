module Utils where

import Tokens


-- | Convert a list of scheme expressions to linkedList of ConsCell
arrayToList :: [Scheme] -> ConsCellData
arrayToList [] = ConsCellData Void Void
arrayToList (x:[]) = ConsCellData x Void
arrayToList (x:xs) = ConsCellData x $ ConsCell $ arrayToList xs


-- | Convert a scheme list to array
listToArray :: ConsCellData -> [Scheme]
listToArray (ConsCellData car Void) = [car]
listToArray (ConsCellData car (ConsCell cdr)) = (car : listToArray cdr)
listToArray (ConsCellData car cdr) = (car:[cdr])


-- | Count occurences
count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)
