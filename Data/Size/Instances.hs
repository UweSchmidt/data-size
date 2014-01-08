{-# LANGUAGE BangPatterns #-}
{-# OPTIONS -fno-warn-orphans  #-}

module Data.Size.Instances
where

import qualified Data.List            as L
import           Data.Size.Base

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
-- import qualified Data.ByteString.Short as SS -- requires bytestring-0.10.4
import           Data.Int
import qualified Data.IntMap          as IM
import qualified Data.IntSet          as IS
import qualified Data.Map             as M
import           Data.Word
import qualified Foreign.Storable     as FS

-- ----------------------------------------

instance Sizeable Bool   where dataOf = dataOfStorable
instance Sizeable Int    where dataOf = dataOfStorable
instance Sizeable Char   where dataOf = dataOfStorable
instance Sizeable Float  where dataOf = dataOfStorable
instance Sizeable Double where dataOf = dataOfStorable

instance Sizeable Word8  where dataOf = dataOfStorable
instance Sizeable Word16 where dataOf = dataOfStorable
instance Sizeable Word32 where dataOf = dataOfStorable
instance Sizeable Word64 where dataOf = dataOfStorable

instance Sizeable Int8   where dataOf = dataOfStorable
instance Sizeable Int16  where dataOf = dataOfStorable
instance Sizeable Int32  where dataOf = dataOfStorable
instance Sizeable Int64  where dataOf = dataOfStorable

dataOfStorable :: FS.Storable a => a -> Bytes
dataOfStorable x
    = mkBytes (FS.sizeOf x) (FS.alignment x)

dataOfBool :: Bytes
dataOfBool
    = dataOfStorable (undefined :: Bool)

dataOfInt :: Bytes
dataOfInt
    = dataOfStorable (undefined :: Int)

dataOfChar :: Bytes
dataOfChar
    = dataOfStorable (undefined :: Char)

dataOfFloat :: Bytes
dataOfFloat
    = dataOfStorable (undefined :: Float)

dataOfDouble :: Bytes
dataOfDouble
    = dataOfStorable (undefined :: Double)

-- --------------------

instance (Sizeable t1, Sizeable t2) => Sizeable (t1, t2) where
    dataOf (_x1, _x2)
        = 2 .*. dataOfPtr

    statsOf xs@(x1, x2)
        = mkStats xs
          <>
          statsOf x1 <> statsOf x2

-- --------------------

instance (Sizeable t1, Sizeable t2, Sizeable t3) =>
         Sizeable (t1, t2, t3) where
    dataOf (_x1, _x2, _x3)
        = 3 .*. dataOfPtr

    statsOf xs@(x1, x2, x3)
        = mkStats xs
          <>
          statsOf x1 <> statsOf x2 <> statsOf x3

-- --------------------

instance (Sizeable t1, Sizeable t2, Sizeable t3, Sizeable t4) =>
         Sizeable (t1, t2, t3, t4) where
    dataOf (_x1, _x2, _x3, _x4)
        = 4 .*. dataOfPtr

    statsOf xs@(x1, x2, x3, x4)
        = mkStats xs
          <>
          statsOf x1 <> statsOf x2 <> statsOf x3 <> statsOf x4

-- --------------------

instance (Sizeable t) => Sizeable (Maybe t) where
    dataOf x
        = case x of
            Just _  -> dataOfPtr
            Nothing -> dataOfSingleton

    statsOf x
        = case x of
            Just x1 -> constrStats "Just"    x <> statsOf x1
            Nothing -> constrStats "Nothing" x

-- --------------------

instance (Sizeable t1, Sizeable t2) => Sizeable (Either t1 t2) where
    dataOf x
        = case x of
            Left  _ -> dataOfPtr
            Right _ -> dataOfPtr

    statsOf x
        = case x of
            Left  x1 -> constrStats "Left"  x <> statsOf x1
            Right x1 -> constrStats "Right" x <> statsOf x1

-- --------------------
--
-- in list statistics the constructors ([] and (:) are not counted
-- just the # of lists and the total # of cells used for all the (:) nodes

instance Sizeable a => Sizeable [a] where
    dataOf xs
        = length xs .*. (dataOfConstr <> (2 .*. dataOfPtr))

    bytesOf             -- Lists are handled as a single object,
        = dataOf        -- all space is already accumulated in dataOf

    statsOf xs
        | null xs
            = mkStats xs

        | nameOf hd `elem` ["Char", "Int", "Double", "Float", "Bool"]
            = mkStats xs
              <>
              len .*. statsOf hd

        | otherwise
            = mkStats xs
              <>
              (mconcat . L.map statsOf $ xs)
        where
          hd  = head xs
          len = length xs

-- --------------------

instance Sizeable IS.IntSet where
    dataOf s
        | IS.null s
            = dataOfSingleton
        | otherwise
            = len .*. (dataOfObj $ dataOfInt <> dataOfInt)
              <>
              (len - 1) .*. (dataOfObj $ dataOfInt <> dataOfInt <> dataOfPtr <> dataOfPtr)
        where
          len = countTips s

          countTips :: IS.IntSet -> Int
          countTips = cnt 0 . IS.elems
              where
                cnt !i []
                    = i
                cnt !i xs@(x : _)
                    = cnt (i + 1) $
                      dropWhile (\ y -> y `div` bitsPerWord == x `div` bitsPerWord) xs

    bytesOf             -- IntSet is handled as a single object,
        = dataOf        -- all space is already accumulated in dataOf

    statsOf
        = mkStats

-- --------------------

instance Sizeable v => Sizeable (IM.IntMap v) where
    dataOf m
        | IM.null m
            = dataOfSingleton
        | otherwise
            = len .*. (dataOfObj $ dataOfInt <> dataOfPtr)
              <>
              (len - 1) .*. (dataOfObj $ dataOfInt <> dataOfInt <> dataOfPtr <> dataOfPtr)
        where
          len = IM.size m

    bytesOf             -- IntMap is handled as a single object,
        = dataOf        -- all space is already accumulated in dataOf

    statsOf m
        = mkStats m
          <>
          IM.foldr' ((<>) . statsOf) mempty m

-- --------------------

instance (Sizeable k, Sizeable v) => Sizeable (M.Map k v) where
    dataOf m
        | M.null m
            = dataOfSingleton
        | otherwise
            = len .*. (dataOfObj $ dataOfInt <> 4 .*. dataOfPtr)
        where
          len   = M.size m

    bytesOf             -- Map is handled as a single object,
        = dataOf        -- all space is already accumulated in dataOf

    statsOf m
        = mkStats m
          <>
          (mconcat . L.map (uncurry statsOfPair) $ M.toList m)
        where
          statsOfPair k v
              = statsOf k <> statsOf v


-- --------------------

{-
The type definition from Data.ByteString:

data BS.ByteString = PS {-# UNPACK #-} !(ForeignPtr Word8) -- payload
                        {-# UNPACK #-} !Int                -- offset
                        {-# UNPACK #-} !Int                -- length
data ForeignPtr a = ForeignPtr Addr# ForeignPtrContents

data ForeignPtrContents
  = PlainForeignPtr !(IORef (Finalizers, [IO ()]))
  | MallocPtr        (MutableByteArray# RealWorld) !(IORef (Finalizers, [IO ()]))
  | PlainPtr         (MutableByteArray# RealWorld)

In the documentation for short bytestrings
"http://hackage.haskell.org/package/bytestring/docs/Data-ByteString-Short.html"
space requirements are described:

ByteString unshared: 9 words; 36 or 72 bytes + word aligned number of bytes

ShortByteString: 4 words; 32 or 64 bytes + word aligned number of bytes

-}

instance Sizeable BS.ByteString where
    dataOf bs
        = dataOfObj $
          (8 .*. dataOfPtr)                             -- 8 words for length field and pointers
          <> (wordAlign $                               -- size of byte sequence
              BS.length bs .*. dataOf (undefined ::Word8)
             )

    statsOf s
        = mkStats s

{- requires bytestring-0.10.4

instance Sizeable SS.ShortByteString where
    dataOf bs
        = dataOfObj $ bytesOf bs

    bytesOf bs
        = (3 .*. dataOfPtr)                             -- 3 words for length field and pointers
          <> (wordAlign $                               -- size of byte sequence
              BS.length bs .*. dataOf (undefined ::Word8)
             )

    statsOf s
        = mkStats s
-}

-- --------------------

{-
  data BL.ByteString = Empty
                     | Chunk {-# UNPACK #-} ! BS.ByteString ByteString
-}

instance Sizeable BL.ByteString where
    nameOf
        = (++ " (lazy)") . typeName

    dataOf bs
        = length cs .*. (dataOfObj $ dataOfPtr <> dataOfPtr)
          <>
          (mconcat . L.map bytesOf $ cs)
        where
          cs = BL.toChunks bs

    bytesOf             -- ByteString is handled as a single object,
        = dataOf        -- all space is already accumulated in dataOf

    statsOf
        = mkStats

-- ------------------------------------------------------------
