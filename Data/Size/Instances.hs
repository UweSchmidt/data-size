{-# LANGUAGE BangPatterns #-}
{-# OPTIONS -fno-warn-orphans #-}

module Data.Size.Instances
where

import qualified Data.List      as L
import           Data.Monoid
import           Data.Size.Base

import qualified Data.IntMap    as IM
import qualified Data.IntSet    as IS
import qualified Data.Map       as M

-- ----------------------------------------

instance Sizeable Bool   where
instance Sizeable Int    where
instance Sizeable Char   where
instance Sizeable Double where

-- --------------------

instance Sizeable a => Sizeable [a] where
    nameof xs
        = listTypeName (nameof (head xs))
    sizeof
        = mconcat . L.map sizeof
    statsof xs
        | null xs
            = mkstats xs "[]" 0

        | nameof hd `elem` ["Char", "Int", "Double", "Float", "Bool"]
            = mkstats xs "[]" 0
              <>
              len .*. mkstats xs "(:)" 2
              <>
              len .*. statsof hd

        | otherwise
            = mkstats xs "[]" 0
              <>
              len .*. mkstats xs "(:)" 2
              <>
              (mconcat . L.map statsof $ xs)
        where
          hd  = head xs
          len = length xs

listTypeName :: String -> String
listTypeName n
    | n == "Char" = "String"
    | otherwise   = "[" ++ n ++ "]"

-- --------------------

instance Sizeable IS.IntSet where
    sizeof s
        | IS.null s
            = mksize 0
        | otherwise
            = len .*. mksize 2
              <>
              (len - 1) .*. mksize 4
        where
          len = countTips s

    statsof s
        | IS.null s
            = mkstats s "Nil" 0
        | otherwise
            = len .*. mkstats s "Tip" 2
              <>
              (len - 1) .*. mkstats s "Bin" 4
        where
          len = countTips s

-- hack: Data.IntSet.Base is hidden, so we have to look into the source
-- and compute the size by hand

countTips :: IS.IntSet -> Int
countTips = cnt 0 . IS.elems
    where
      cnt !i []
          = i
      cnt !i xs@(x : _)
          = cnt (i + 1) $ dropWhile (\ y -> y `div` bitsPerWord == x `div` bitsPerWord) xs

-- --------------------

instance Sizeable v => Sizeable (IM.IntMap v) where
    sizeof m
        | IM.null m
            = mksize 0
        | otherwise
            = len .*. mksize 2
              <>
              (len - 1) .*. mksize 4
        where
          len = IM.size m

    statsof m
        | IM.null m
            = mkstats m "Nil" 0
        | otherwise
            = len .*. mkstats m "Tip" 2
              <>
              (len - 1) .*. mkstats m "Bin" 4
              <>
              IM.foldr' ((<>) . statsof) mempty m
        where
          len = IM.size m

-- --------------------

instance (Sizeable k, Sizeable v) => Sizeable (M.Map k v) where
    sizeof m
        | M.null m
            = mksize 0
        | otherwise
            = len .*. mksize 5
              <>
              M.foldWithKey (\ k v st -> sizeof k <> sizeof v <> st) mempty m
        where
          len   = M.size m

    statsof m
        | M.null m
            = mkstats m "Tip" 0
        | otherwise
            = (len + 1) .*. mkstats m "Tip" 0
              <>
              len .*. mkstats m "Bin" 5
              <>
              M.foldWithKey (\ k v st -> statsof k <> statsof v <> st) mempty m
        where
          len = M.size m

-- ------------------------------------------------------------