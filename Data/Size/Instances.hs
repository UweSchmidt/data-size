{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS -fno-warn-orphans #-}

module Data.Size.Instances
where

import qualified Data.List            as L
import           Data.Size.Base

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.IntMap          as IM
import qualified Data.IntSet          as IS
import qualified Data.Map             as M

import qualified Foreign.Storable     as FS

-- ----------------------------------------

instance Sizeable Bool   where bytesOf = bytesOfStorable
instance Sizeable Int    where bytesOf = bytesOfStorable
instance Sizeable Char   where bytesOf = bytesOfStorable
instance Sizeable Float  where bytesOf = bytesOfStorable
instance Sizeable Double where bytesOf = bytesOfStorable

bytesOfStorable :: FS.Storable a => a -> Bytes
bytesOfStorable x = sizeOfObj $ mkBytes (FS.sizeOf x) (FS.alignment x)

-- --------------------

instance (Sizeable t1, Sizeable t2) => Sizeable (t1, t2) where
    nameOf (x1, x2)
           = concat ["("
                    , nameOf x1
                    , ","
                    , nameOf x2
                    , ")"
                    ]
    bytesOf (_x1, _x2)
        = sizeOfObj $ sizeOfPtr <> sizeOfPtr

    statsOf xs@(x1, x2)
        = mkStats xs ""
          <>
          statsOf x1 <> statsOf x2

-- --------------------

instance (Sizeable t1, Sizeable t2, Sizeable t3) =>
         Sizeable (t1, t2, t3) where
    nameOf (x1, x2, x3)
           = concat ["("
                    , nameOf x1
                    , ","
                    , nameOf x2
                    , ","
                    , nameOf x3
                    , ")"
                    ]
    bytesOf (_x1, _x2, _x3)
        = sizeOfObj $ sizeOfPtr <> sizeOfPtr <> sizeOfPtr

    statsOf xs@(x1, x2, x3)
        = mkStats xs ""
          <>
          statsOf x1 <> statsOf x2 <> statsOf x3

-- --------------------

instance (Sizeable t1, Sizeable t2, Sizeable t3, Sizeable t4) =>
         Sizeable (t1, t2, t3, t4) where
    nameOf (x1, x2, x3, x4)
           = concat ["("
                    , nameOf x1
                    , ","
                    , nameOf x2
                    , ","
                    , nameOf x3
                    , ","
                    , nameOf x4
                    , ")"
                    ]
    bytesOf (_x1, _x2, _x3, _x4)
        = sizeOfObj $ sizeOfPtr <> sizeOfPtr <> sizeOfPtr <> sizeOfPtr

    statsOf xs@(x1, x2, x3, x4)
        = mkStats xs ""
          <>
          statsOf x1 <> statsOf x2 <> statsOf x3 <> statsOf x4

-- --------------------

instance (Sizeable t) => Sizeable (Maybe t) where
    nameOf x
        = unwords ["Maybe", nameOf x1]
          where
            Just x1 = case x of
                        Just _  -> x
                        Nothing -> Just undefined

    bytesOf x
        = case x of
            Just _  -> sizeOfObj $ sizeOfPtr
            Nothing -> sizeOfSingleton
                       
    statsOf x
        = case x of
            Just x1 -> mkStats x "Just"    <> statsOf x1
            Nothing -> mkStats x "Nothing"
                       
-- --------------------
{-
instance Sizeable a => Sizeable [a] where
    nameOf xs
        = listTypeName (nameOf (head xs))
    bytesOf []
            = sizeOfSingleton
    bytesOf (x : xs)
            = sizeOfObj $ sizeOfPtr <> sizeOfPtr
    objectsOf
        = mconcat . L.map objectsOf
    statsOf xs
        | null xs
            = mkStats xs "[]"

        | nameOf hd `elem` ["Char", "Int", "Double", "Float", "Bool"]
            = mkStats xs "[]" 0
              <>
              len .*. mkStats xs "(:)" 2
              <>
              len .*. statsOf hd

        | otherwise
            = mkStats xs "[]" 0
              <>
              len .*. mkStats xs "(:)" 2
              <>
              (mconcat . L.map statsOf $ xs)
        where
          hd  = head xs
          len = length xs

listTypeName :: String -> String
listTypeName n
    | n == "Char" = "String"
    | otherwise   = "[" ++ n ++ "]"
-- -}
-- --------------------
{-
instance Sizeable IS.IntSet where
    objectsOf s
        | IS.null s
            = mksize 0
        | otherwise
            = len .*. mksize 2
              <>
              (len - 1) .*. mksize 4
        where
          len = countTips s

    statsOf s
        | IS.null s
            = mkStats s "Nil" 0
        | otherwise
            = len .*. mkStats s "Tip" 2
              <>
              (len - 1) .*. mkStats s "Bin" 4
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
    objectsOf m
        | IM.null m
            = mksize 0
        | otherwise
            = len .*. mksize 2
              <>
              (len - 1) .*. mksize 4
        where
          len = IM.size m

    statsOf m
        | IM.null m
            = mkStats m "Nil" 0
        | otherwise
            = len .*. mkStats m "Tip" 2
              <>
              (len - 1) .*. mkStats m "Bin" 4
              <>
              IM.foldr' ((<>) . statsOf) mempty m
        where
          len = IM.size m

-- --------------------

instance (Sizeable k, Sizeable v) => Sizeable (M.Map k v) where
    objectsOf m
        | M.null m
            = mksize 0
        | otherwise
            = len .*. mksize 5
              <>
              M.foldWithKey (\ k v st -> objectsOf k <> objectsOf v <> st) mempty m
        where
          len   = M.size m

    statsOf m
        | M.null m
            = mkStats m "Tip" 0
        | otherwise
            = (len + 1) .*. mkStats m "Tip" 0
              <>
              len .*. mkStats m "Bin" 5
              <>
              M.foldWithKey (\ k v st -> statsOf k <> statsOf v <> st) mempty m
        where
          len = M.size m

-- --------------------

{-
data BS.ByteString = PS {-# UNPACK #-} !(ForeignPtr Word8) -- payload
                        {-# UNPACK #-} !Int                -- offset
                        {-# UNPACK #-} !Int                -- length
-}

instance Sizeable BS.ByteString where
    objectsOf
        = mksize . (3 +) . bytesToWords . BS.length

    statsOf s
        = mkStats s "" (dataSize . objectsOf $ s)

-- --------------------

{-
  data BL.ByteString = Empty
                     | Chunk {-# UNPACK #-} ! BS.ByteString ByteString
-}

instance Sizeable BL.ByteString where
    objectsOf
        = mconcat . L.map objectsOf' . BL.toChunks
          where
            objectsOf' c = mksize (3 + 1 + bytesToWords (BS.length c))

    statsOf s
        = mkStats s "" (dataSize . objectsOf $ s)

-- ------------------------------------------------------------
-- -}
