{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}

module Indexable ((:.), Indexable, (&?), (&!), (&), getIdx, fromLst) where 

import qualified Data.Map.Strict as M (Map, (!?), fromList)
import qualified Data.List as L ((!!),nub)
import Data.Typeable (Typeable, eqT, (:~:)( Refl ))
import Data.Functor (fmap)
-- import CommonTools ((##), intg2Int)
import ArraySyntax (arrayParse, textArrParse)
import Data.Proxy
import Data.Functor.Compose
import qualified Data.HashMap.Strict as HM (HashMap, lookup, fromList)
import Data.Hashable (Hashable)
import qualified Data.IntMap as IM (IntMap, (!?), fromList)
import qualified Data.Sequence as S (Seq, fromList, (!?))
import qualified Data.Vector as V (Vector, fromList, (!?))


-- | Indexable class definition, generalize action of slicing, subsetting and
-- | index selection.
-- | Two minimum function: 
-- |   (&?) get Maybe element from given key/index
-- |   fromLst build the indexiable object from a list
-- | Main action (&) slicing operator, which can be used to slicing arbitrary
-- | dimension indexable object

class (Foldable f, Typeable b, Eq b, Read b) => Indexable f b | f -> b where
    -- | minimal operator, get element from key
    -- get elements
    (&?) :: f a -> b -> Maybe a
    -- form container from list
    fromLst :: (Typeable b) => [(b,a)] -> f a

    -- | all derived functions
    -- force element or error
    (&!) :: f a -> b -> a
    (&!) xs key = case (xs&?key) of
                    Just v -> v
                    Nothing -> error "key/index error"

    -- get all elements in list
    getIdx :: f a -> [b] -> f a
    getIdx xs keys = 
        let keys0 = L.nub keys
            resMaybes = (xs&?) <$> keys0
            zipped = zip keys0 resMaybes
            resJusts = filter justVal zipped
            res = rmJusts <$> resJusts
        in  fromLst res
        where justVal (a, b) =
                case b of
                  Nothing -> False
                  Just _  -> True
              rmJusts (a, Just b) = (a, b)

    -- slicing by string
    -- backward slicing
    -- bwSlice :: (Show (f a), Show a) => f a -> [String] -> (f a, [String])
    bwSlice :: f a -> [String] -> (f a, [String])
    bwSlice !xs !slice  
        | length slice == 0 = (xs, [])   -- ## "base bwSlice case 1: "
        | s:ls <- slice = (xs&[s], ls)   -- ## "base bwSlice case 2: "


    -- normal slicing
    -- (&) :: (Show (f a), Show a) => f a -> [String] -> f a
    (&) :: f a -> [String] -> f a
    (&) !xs !slice
        | length slice == 0 = xs        -- ## "bottom case1: "
        | length slice >1 = error "to many elements, only allow one"
        | (slice!!0) == "" = xs
        | otherwise =
            let arrRes = arrayParse ":" l (slice!!0)  -- ## "arrRes: "
                strArrRes = textArrParse ":" l (slice!!0) -- ##"strArrRes: "
            in
            case arrRes of
              Just arr -> getIdx xs (toType <$> show <$> arr) -- ##"getIdx1: "
              Nothing ->
                case strArrRes of
                  Just strArr -> getIdx xs (toType <$> strArr) -- ##"getIdx2: "
                  Nothing -> fromLst []              -- ##"Nothing: "
        where l = length xs
              toType x = 
                case eqT :: Maybe (b :~: String) of
                  Just Refl -> x
                  Nothing   -> (read x)::b

-- | recursive slicing on arbirary nested indexable objects
instance (Indexable f a, Indexable g b, Functor f) => Indexable (Compose f g) (a,b) where
    (&?) :: (Compose f g) v -> (a,b) -> Maybe v
    (&?) comps (key1,key2) = 
            let maps = getCompose comps 
                res0 = maps&?key1
            in  case res0 of
                  Just maps1 -> maps1&?key2
                  Nothing -> Nothing

    fromLst :: [((a,b),v)] -> (Compose f g) v
    fromLst kvLst = 
      let reorder ((a,b),v) = (a,(b,v))
          kvLst0 = reorder <$> kvLst
          keys1 = L.nub $ fst <$> kvLst0
          sglLayer xs a = snd <$> filter (\x-> fst x==a) xs
          innerLst = (sglLayer kvLst0) <$> keys1
          fromInnerLst = fromLst <$> innerLst
          outerLst = zip keys1 fromInnerLst
          fromOuterLst = fromLst outerLst
      in  Compose fromOuterLst

    -- | overload slicing function
    -- backward slicing
    bwSlice :: (Compose f g) v -> [String] -> ((Compose f g) v, [String])
    bwSlice !comps !slice
        | length slice == 0 = (comps, [])
        | otherwise =
            let mmap = getCompose comps
                (bwComps, bwSl) = bwSlice mmap slice
            in  case length bwSl of
                  0 -> (Compose bwComps, [])            -- ## "rec bwSlice case1: "
                  _ -> let mysl = take 1 bwSl           -- ## "mysl: "
                           res = (&mysl) <$> bwComps    -- ## "res: "
                       in  (Compose res, tail bwSl)     -- ## "rec bwSlice case2: "


    (&) :: (Compose f g) v -> [String] -> (Compose f g) v
    (&) comps slice
          | length slice == 0 = comps
          | otherwise = fst $ bwSlice comps slice

-- | Indexable instance: list with integer key
instance Indexable [] Integer where
    (&?) vLst key =
          let  key1 = intg2Int key in
          case key1 < (length vLst) && key1 >= 0 of
            True -> Just $ vLst L.!! key1
            False -> Nothing

    -- only take the value part
    fromLst kvLst = snd <$> kvLst

-- | Indexable instance: Data.Map.Strict
instance (Typeable k, Ord k, Read k) => Indexable (M.Map k) k where
    (&?) vMap key = vMap M.!? key

    fromLst vLst = M.fromList vLst

-- | Indexable instance: Data.HashMap.Strict
instance (Typeable k, Read k, Eq k, Hashable k) => Indexable (HM.HashMap k) k where
  (&?) vMap key = HM.lookup key vMap
  fromLst vLst = HM.fromList vLst

-- | Indexable instance: Data.IntMap.Strict
instance Indexable IM.IntMap Integer where
  (&?) vMap key = 
          let key1 = intg2Int key in
          vMap IM.!? key1

  fromLst vLst = 
      let myfun (a,b) = (intg2Int a, b)
      in  IM.fromList $ myfun <$> vLst

-- | Indexable instance: Data.Sequence
instance Indexable S.Seq Integer where
  (&?) vSeq key = 
          let key1 = intg2Int key in
          vSeq S.!? key1

  fromLst vLst = 
      let myfun (a,b) = b
      in  S.fromList $ myfun <$> vLst

-- | Indexable instance: Data.Vector
instance Indexable V.Vector Integer where
  (&?) vVec key = 
          let key1 = intg2Int key in
          vVec V.!? key1

  fromLst vLst = 
      let myfun (a,b) = b
      in  V.fromList $ myfun <$> vLst


-- | type composition
type f:.g = Compose f g


-- | from integral to int
intg2Int :: (Integral a)=> a -> Int  
intg2Int aVal = fromIntegral aVal::Int

