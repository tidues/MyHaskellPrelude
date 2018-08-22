{-# LANGUAGE BangPatterns #-}

module DFrame where

import qualified Data.Map.Strict as M (Map, empty, insert, fromList, map, elems)
import Data.List.Split(splitOneOf, splitOn)
import CommonTools
import Data.Functor.Compose 
import Indexable
import Data.Typeable (Typeable)

-- | read table into two level map
type RowSlice = String
type RowSeps = String
type ColSeps = String

-- | break input string into two dimensional list
str2Lists :: String -> RowSlice -> RowSeps -> ColSeps ->  [[String]]
str2Lists str slice rSeps cSeps =
    let rows = splitOneOf rSeps str  -- ## "rows: "
        infoLst = filter (\x -> length x > 0) rows&[slice] -- ## "infoLst: "
    in  (splitRow cSeps) <$> infoLst

-- | split each row into list of fields
splitRow :: ColSeps -> String -> [String]
splitRow seps str = filter (\x -> length x>0) (strStrip <$> splitOneOf seps str)

type KeyCol = Int
type Headers = [String]
type RawFrame a = Compose (M.Map a) (M.Map String) String
data DFrame a = DFrame {getDf :: RawFrame a,
                        header :: [String],
                        index :: [a]
                        }

instance (Show a, Typeable a, Ord a, Read a) => Show (DFrame a) where
  show df = 
    let !hLst = header df   -- ## "\nheaders: "
        !idLst = index df   -- ## "\nindexes: "
        hLstPd = padList dfLen hLst
        !headerStr = formRow "" hLstPd   -- ## "\nheaderStr: "
        !hLst1 = drop 1 hLst             -- ## "\nheaderList1: "
        !mmap = getCompose (getDf df)     -- ##  "\nmmap: "
        !strMap = M.map ((formRow "").(padList (drop 1 dfLen)).(getVals hLst1)) mmap    -- ## "\nstrMap"
        pdIdx = (\x -> padding R x (dfLen!!0)) <$> toString<$>idLst
        strLst = zipWith (\x y -> x++""++y) pdIdx (getVals idLst strMap)
        contents = formRow "\n" strLst
    in  headerStr ++ "\n" ++ contents
    where getVals !kLst !vMap = (\x-> (vMap&!x)) <$> kLst -- ## "\n getVals: "
          padList lenLst strLst = zipWith (padding R) strLst lenLst
          dfLen = colLength df
          formRow sep sLst = 
            let tmpStr = foldr (\a b -> a ++ sep ++ b) "" sLst
                sLen = length tmpStr
            in  take (sLen-1) tmpStr

-- | get all lengths of a dataframe
colLength :: (Read a, Ord a, Typeable a, Show a) => DFrame a -> [Int]
colLength df = let hLst = header df
                   !hLst1 = hLst&["1:"]     -- ## "hLst1: "
                   rf = getDf df
                   !lst0 = (maxLen rf) <$> hLst1  -- ## "lst0: "
                   idLst =  index df
                   !lId = max 2 (maximum $ length <$> toString <$> idLst) -- ## "lId: "
                in lId : lst0


-- | padding string to certain length
data Dir = L | R
padding :: Dir -> String -> Int -> String
padding dir str len0 =
  let len = len0 + 2 in
  case length str >= len of
    True -> str
    False ->
      let pd = replicate (len-(length str)) ' '
      in  case dir of
            L -> str ++ pd
            R -> pd ++ str

-- | get length of each column
maxLen :: (Read a, Ord a, Typeable a, Show a) => RawFrame a -> ColName -> Int
maxLen rf cName = let !cLst = colFlat rf cName  -- ## "cLst: "
                      !lLst = length <$> cLst   -- ## "lLst: "
                  in  max (maximum lLst) (length cName)


-- | flatten each column (the column of the DF)
type ColName = String
colFlat :: (Read a, Ord a, Typeable a, Show a) => RawFrame a -> ColName -> [String]
colFlat rf cName= let !col = rf&["",cName]        -- ## "col: "
                      !mcol = getCompose col   -- ## "mcol: "
                      !mapVals = M.elems mcol  -- ## "mapVals: "
                  in  extractVal <$> mapVals
                  where extractVal mmap =
                          (M.elems mmap)!!0
                 

-- | from key list to values
getValList :: [String] -> M.Map String String -> [String]
getValList kLst vmap = (\x-> vmap&!x) <$> kLst

-- | construct dataframe with given column as key
mkDFWithKey :: (Ord a, Show a, Read a) => Headers -> KeyCol -> a -> [[String]] -> DFrame a
mkDFWithKey hLst keyCol aVal strLst =
    let dfLst = (mkRowMap hLst keyCol aVal) <$> strLst
        hLstNew = colReorder hLst keyCol
    in DFrame (Compose $ M.fromList dfLst) hLstNew (toa<$>(!!keyCol)<$>strLst)  
    where toa x = load x aVal

-- | Create the inner dimensional rows
mkRowMap :: (Ord a, Show a, Read a) => Headers -> KeyCol -> a -> [String] -> (a, M.Map String String)
mkRowMap !hLst !keyCol aVal !strLst =
    let !zipped = zip hLst strLst -- ## "zipped: "
        -- !tmpLst = (zipped&[":"++(show keyCol)]) ++ (zipped&[(show $ keyCol+1)++":"]) -- ## "tmpLst: "
        !tmpLst = colReorder zipped keyCol 
        in  (load (strLst!!keyCol) aVal, M.fromList tmpLst )

-- | columns reorder
colReorder :: [a] -> KeyCol -> [a]
colReorder aLst keyCol = aLst&[":"++(show keyCol)] ++ (aLst&[(show $ keyCol+1)++":"])

-- | construct dataframe with new col as index
mkDFNewKey :: Headers -> [[String]] -> DFrame Int
mkDFNewKey !hLst !strLst =
    let idLst0 = [1..(length strLst)]  -- ## "\nidLst0: "
        !idLst = show <$> idLst0   -- ## "\nidLst: "
        !strLst1 = zipWith (:) idLst strLst     -- ## "\nstrLst1: "   
        !hLst1 = "ID":hLst
        !dfLst = (mkRowMap hLst1 0 0) <$> strLst1 -- ## "\ndfLst: "
    in DFrame (Compose $ M.fromList dfLst) (hLst1) idLst0

-- | construct dataframe with first row as header
mkDFWithHeadKey :: (Ord a, Show a, Read a) => KeyCol -> a -> [[String]] -> DFrame a
mkDFWithHeadKey kCol aVal strLst =
  let hLst = strLst&!0
      vLst = strLst&["1:"]
  in  mkDFWithKey hLst kCol aVal vLst

-- | construct dataframe with first row as header and new index
mkDFHeadNewKey :: [[String]] -> DFrame Int
mkDFHeadNewKey strLst =
  let hLst = strLst&!0     -- ## "\nhLst: "
      vLst = strLst&["1:"]  -- ## "\nvLst: "
  in  mkDFNewKey hLst vLst



