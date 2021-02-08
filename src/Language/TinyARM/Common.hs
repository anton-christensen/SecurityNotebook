module Language.TinyARM.Common where

import Numeric
import Data.Bits

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S

-- Maps on a map using both keys and values
mapmap :: (Ord k2) => ((k1, v1) -> (k2,v2)) -> M.Map k1 v1 -> M.Map k2 v2 
mapmap f m = M.fromList $ map f $ M.toList m

setmapReplace :: (Ord b) => (a -> [b]) -> S.Set a -> S.Set b
setmapReplace f s = S.fromList $ concat $ map f $ S.toList s

-- invert :: (Ord k, Ord v) => M.Map k v -> M.Map [v] k
invert :: Ord v => M.Map k v -> M.Map v [k]
invert m = M.fromListWith (++) pairs
    where pairs = [(v, [k]) | (k, v) <- M.toList m]


-- Maps on a map using both keys and values but also allows inserting new (k, a) pairs
mapmapReplace :: (Ord k2) => ((k1, v1) -> [(k2,v2)]) -> M.Map k1 v1 -> M.Map k2 v2
mapmapReplace f m = M.fromList $ concat $ map f $ M.toList m

showHexWord :: (Integral a, Show a, FiniteBits a) => a -> String
showHexWord i = "0x"++(leftpad hexNum '0' hexLength)
    where 
        hexLength = div (finiteBitSize i) 4
        hexNum = (showHex i "")

leftpad :: String -> Char -> Int -> String
leftpad str pad maxLen = (_padding str pad maxLen) ++ str

rightpad :: String -> Char -> Int -> String
rightpad str pad maxLen = str ++ (_padding str pad maxLen)

_padding :: String -> Char -> Int -> String 
_padding str pad maxLen = concat $ take (max 0 (maxLen - (length str))) $ repeat [pad]

showBits :: (Integral a, FiniteBits a) => a -> String
showBits val = foldl (\s bit -> (if bit then '1' else '0'):s) "" bitString
  where 
    highestBit = (finiteBitSize val) - 1
    bitString = map (testBit val) [0..highestBit]

replaceInString :: String -> String -> String -> String
replaceInString substring replacement str = 
    T.unpack $ T.replace (T.pack substring) (T.pack replacement) $ T.pack str

escapeQuote :: String -> String
escapeQuote str = replaceInString "\"" "\\\"" $ replaceInString "\\\"" "\\\\\"" str

indent :: Int -> String -> String
indent n s = 
    let spaces = concat $ take n $ repeat " "
    in spaces ++ (replaceInString "\n" ("\n"++spaces) s)

tabWidth :: Int
tabWidth = 2

extractJustContent :: Maybe a -> a
extractJustContent (Just x) = x


printListType :: String -> String -> String -> Int -> (t a -> [a]) -> (a -> String) -> t a -> String
printListType beg sep end indentSize toList printer listLike = beg ++ (indent indentSize $ (L.intercalate sep (map printer $ toList listLike))) ++ end

printMultilineSet :: (s -> String) -> S.Set s -> String
printMultilineSet printer m = "{\n" ++ (indent tabWidth $ (L.intercalate ",\n" (map printer $ S.toList m))) ++ "\n}"

printMultilineMap :: ((k, v) -> String) -> M.Map k v -> String
printMultilineMap printer m = "[\n" ++ (indent tabWidth $ (L.intercalate ",\n" (map printer $ M.toList m))) ++ "\n]"

printInlineSet :: (s -> String) -> S.Set s -> String
printInlineSet printer m = "{ " ++ (L.intercalate ", " (map printer $ S.toList m)) ++ " }"

printInlineMap :: ((k, v) -> String) -> M.Map k v -> String
printInlineMap printer m = "[ " ++ (L.intercalate ", " (map printer $ M.toList m)) ++ " ]"
