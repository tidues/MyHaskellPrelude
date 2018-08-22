{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables#-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators, GADTs, DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

module CommonTools where

import Data.Text(pack, unpack, strip)
import System.IO (readFile)
import Control.Exception.Base
import System.Process(readProcess)
import Debug.Trace
import Data.Typeable
import Data.Proxy
import Data.HList.HArray
import Data.HList
import GHC.TypeNats
import qualified Data.Map.Strict as M (Map, empty, insert, fromList)
import Data.List.Split(splitOneOf, splitOn)
-- import Indexable

-- | force either
getEither :: (Show e, Show a) => Either e a -> a
getEither val =
    case val of
        Left lVal -> error $ "getEither " ++ (show val) ++ ":" ++ (show lVal)
        Right res -> res

-- | String to all readable types
load ::(Read a) => String -> a -> a
load str v = asTypeOf (read str) v


-- | general to string
toString :: (Show a, Typeable a) => a -> String
toString val =  if sameType val "" then (castTo val "") else (show val)

-- | strip strings
strStrip :: String -> String
strStrip = unpack.strip.pack

-- | concat with
concatWith :: String -> [String] -> String
concatWith sep strLst =
    let res = foldr (\x y -> x ++ sep ++ y) "" strLst
    in  take ((length res)-1) res

-- | parse input data or string
type FileName = String
type Content = String

strParser :: String -> (Content -> a) -> IO a
strParser str parser = do
    return $ parser str

dataReader :: FileName -> IO Content
dataReader fname = do
    content <- readFile fname
    return content

dataParser :: FileName -> (Content -> a) -> IO (Either IOException a)
dataParser fname parser = try $ do
    content <- readFile fname
    strParser content parser

-- | run a script and get result
readCmd :: FileName -> [String] -> IO String
readCmd fpath args = readProcess fpath args ""

-- | trace functions
debug :: (Show a) => a -> String -> a
debug x str = trace ("\n"++str ++ (show x)) x

infixl 0 ##
(##) x str = debug x str

-- | safe downcast value explicitly
castTo :: forall a b . (Typeable a, Typeable b) => a -> b -> b
castTo x y = case eqT :: Maybe (a :~: b) of
                Just Refl -> x
                Nothing -> error "castTo error" 

-- | test if same type
sameType :: forall a b . (Typeable a, Typeable b) => a -> b -> Bool
sameType x y = case eqT :: Maybe (a :~: b) of
                Just Refl -> True
                Nothing -> False


-- | get ith element from HList
type family NattHNat (n::Nat) :: HNat where
    NattHNat 0 = HZero
    NattHNat n = HSucc (NattHNat (n-1))

nattHNat :: Proxy (n::Nat) -> Proxy (NattHNat n)
nattHNat Proxy = Proxy
(!!!) :: HLookupByHNat (NattHNat n) l => 
         HList l -> Proxy (n::Nat) -> HLookupByHNatR (NattHNat n) l
hLst !!! px = hLookupByHNat (nattHNat px) hLst

-- | test if the string is a integral number
isDigit :: Char -> Bool
isDigit c = elem c "0123456789"

isIntegral :: String -> Bool
isIntegral str 
    | length str == 0 = False 
    | (str!!0) == '0' && length str == 1 = True
    | (str!!0) == '0' = False
    | (str!!0) == '-' = all isDigit (tail str)
    | otherwise = all isDigit str

-- | from integral to int
intg2Int :: (Integral a)=> a -> Int  
intg2Int aVal = fromIntegral aVal::Int
