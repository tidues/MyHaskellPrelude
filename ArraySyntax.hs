{-# LANGUAGE BangPatterns #-}
module ArraySyntax (ArrayStr, arrayParse, arrayLstParse,
                    textArrParse, textArrLstParse,
                    makeArr, makeArrLst) where

import Data.IntSet as IS (insert, fromList)
import Data.List.Split(splitOneOf, splitOn)
import Data.List(elemIndex)
import Control.Monad (join)
import CommonTools
import Data.Traversable (traverse)

type ArrayStr = String
type SepStr = String
type TotalLen = Int

-- | parse array notation: split by "," and range by "-"
arrayParse :: SepStr -> TotalLen -> ArrayStr -> Maybe [Int]
arrayParse seps k arrStr = 
    let strLst = splitOn "," arrStr
    in  arrayLstParse seps k strLst 

-- | parse array list
arrayLstParse :: SepStr -> TotalLen -> [String] -> Maybe [Int]
arrayLstParse !seps !k !strLst =
    let !arrLst = strStrip <$> strLst    -- ## "arrLst: "
    in  join <$> traverse (progParse seps k) arrLst  -- ## "joinRes: "

-- parse each element in an array
progParse :: SepStr -> TotalLen -> String -> Maybe [Int]
progParse !seps !k !str =
  case elem (seps!!0) str of
    False -> case isIntegral str of
              True -> Just [load str 0]
              False -> Nothing
    True -> 
      let !ht = splitOneOf seps str         -- ## "ht: "
          !rmLast | k>=0 && (seps!!0)==':' = True  -- ## "rmLstTrue: "
                 | otherwise = False        -- ## "rmLstFalse: "
      in  splitSlice k rmLast ht            -- ## "progRes: "

-- | logic test of the split syntax
splitSlice :: TotalLen -> Bool -> [String] -> Maybe [Int]
splitSlice !k !rmLast !slice
  | l<2 || l >3 = Nothing     -- ## "Case1: "
  | (slice!!1) == "" && k<0 = Nothing   -- ## "Case2: "
  | (all intOrEmp slice) =
      let !from | (slice!!0) == "" = 0     
               | otherwise = load (slice!!0) 0   
          !step | l==2 = 1                     -- ## "step1: "
               | (slice!!2) == "" = 1         -- ## "step2: "
               | otherwise = load (slice!!2) 0   -- ## "step3: "
          !to1  | (slice!!1) == "" = k           -- ## "to11: "
               | k>=0 && (slice!!1)!!0 == '-' =
                  k + (load (slice!!1) 0)       -- ## "to12: "
               | otherwise =
                  let to0 = load (slice!!1) 0   -- ## "to0: "
                  in  case k>=0 of
                        False -> to0
                        True  ->
                          case to0 < k of
                            True -> to0
                            False -> k
          !to   | rmLast = to1-1         -- ## "to1: "
               | otherwise = to1        -- ## "to2: "
      in  Just [from,(from+step)..to]   -- ## "Case3: "
  | otherwise = Nothing                 -- ## "Case4: "
  where !l = length slice               -- ## "l: "
        intOrEmp !x = x == "" || isIntegral x -- ## "intOrEmp: "


-- | build array notation from list of int
type GapBound = Int

makeArr :: GapBound -> [Int] -> String
makeArr bd intLst = 
    let strLst = makeArrLst bd intLst
    in  concatWith "," strLst

-- | build list of array notations

makeArrLst :: GapBound -> [Int] -> [String]
makeArrLst bd intLst = progToArr <$> progLst bd intLst

-- | trasform single progression list into array notation
progToArr :: [Int] -> String
progToArr intLst =
    case length intLst of
        0 -> ""
        1 -> show $ intLst!!0
        2 -> (show $ intLst!!0) ++ "," ++ (show $ intLst!!1)
        _ -> let hd = show $ intLst!!0
                 tl = show $ last intLst 
                 step = 
                    let diff = intLst!!1 - intLst!!0
                    in case diff of
                        1 -> ""
                        _ -> ":" ++ (show diff)
             in hd ++ "-" ++ tl ++ step

-- | split list into arithmetic progressions
progLst :: GapBound -> [Int] -> [[Int]]
progLst bd intLst = splitProg bd [] intLst

-- | transfer set into array notation
splitProg :: GapBound -> [[Int]] -> [Int] -> [[Int]]
splitProg bd resLst intLst = 
    let (prog, rest) = getProg (-1) bd intLst
        newRes = resLst ++ [prog] 
    in case rest of
        [] -> newRes
        _  -> splitProg bd newRes rest

-- | split on the first set of elements that form an arithmetic progression
type Gap = Int

getProg :: Gap -> GapBound -> [Int] -> ([Int],[Int])
getProg _ _ [] = ([],[])
getProg _ _ [a] = ([a],[])
getProg (-1) _ [a,b] = ([a],[b])
getProg (-1) bd (a:b:c:xs) =
    let k = b-a
    in case (c-b == k && k <= bd) of
        True -> 
            let res = getProg k bd (c:xs)
            in  ([a,b] ++ (fst res), snd res)
        False -> ([a], b:c:xs)
getProg k bd (a:b:xs) =
    case b-a == k of
        True -> 
            let res = getProg k bd (b:xs)
            in  ([a] ++ (fst res), snd res)
        False -> ([a], b:xs)
        
-- | parse text contain array notation
textArrParse :: SepStr -> TotalLen -> ArrayStr -> Maybe [String]
textArrParse seps k arrStr =
    let arrLst = strStrip <$> splitOn "," arrStr
    in textArrLstParse seps k arrLst 

textArrLstParse :: SepStr -> TotalLen -> [String] -> Maybe [String]
textArrLstParse seps k strLst = join <$> traverse (textProgParse seps k) strLst

textProgParse :: SepStr -> TotalLen -> String -> Maybe [String]
textProgParse seps k str = 
    case elemIndex '[' str of
        Just k1 ->
            let Just !k2 = elemIndex ']' str      -- ## "k2: "
                !myHead = str&[Just 0, Just k1]      -- ## "myHead: "
                !myArr = str&[Just $ k1+1, Just k2]  -- ## "myArr: "
                !myTail = str&[Just $ k2+1, Nothing]  -- ## "myTail: "
                !resLst = progParse seps k myArr     -- ## "resLst: "
            in case resLst of
                Nothing -> Nothing
                Just xs -> 
                  let arrLst = show <$> xs
                  in Just $ (\x -> myHead ++ x ++ myTail) <$> arrLst
        Nothing -> Just [str]

-- | local function for parsing slicing, so no general treatment and error testing
(&) :: [a] -> [Maybe Int] -> [a]
(&) !lst !slice = 
     let !hd | hd0 == Nothing = 0
             | Just k <- hd0 = k
         !tl | tl0 == Nothing = length lst
             | Just k <- tl0 = k
      in take (tl-hd) (drop hd lst)
      where !hd0 = slice!!0  -- ## "hd0: "
            !tl0 = slice!!1  -- ## "tl0: "
