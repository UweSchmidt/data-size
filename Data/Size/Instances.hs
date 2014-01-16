{-# LANGUAGE BangPatterns #-}
{-# OPTIONS -fno-warn-orphans  #-}

module Data.Size.Instances
where

import qualified Data.List                      as L

import qualified Data.ByteString.Internal       as BS
import qualified Data.ByteString.Lazy.Internal  as BL
import qualified Data.ByteString.Short          as SS
import           Data.Int
import qualified Data.IntMap                    as IM
import qualified Data.IntSet                    as IS
import qualified Data.Map.Strict                as M
import           Data.Size.Base
import qualified Data.Text.Internal             as T (Text (..))
import qualified Data.Text.Lazy.Internal        as TL
import           Data.Typeable
import           Data.Word

import qualified Foreign.Storable              as FS

-- ----------------------------------------

instance Sizeable Bool   where
    dataOf  = dataOfStorable
    statsOf = mkStats

instance Sizeable Int    where
    dataOf  = dataOfStorable
    statsOf = mkStats

instance Sizeable Char   where
    dataOf  = dataOfStorable
    statsOf = mkStats

instance Sizeable Float  where
    dataOf  = dataOfStorable
    statsOf = mkStats

instance Sizeable Double where
    dataOf  = dataOfStorable
    statsOf = mkStats


instance Sizeable Word8  where
    dataOf  = dataOfStorable
    statsOf = mkStats

instance Sizeable Word16 where
    dataOf  = dataOfStorable
    statsOf = mkStats

instance Sizeable Word32 where
    dataOf  = dataOfStorable
    statsOf = mkStats

instance Sizeable Word64 where
    dataOf  = dataOfStorable
    statsOf = mkStats


instance Sizeable Int8   where
    dataOf  = dataOfStorable
    statsOf = mkStats

instance Sizeable Int16  where
    dataOf  = dataOfStorable
    statsOf = mkStats

instance Sizeable Int32  where
    dataOf  = dataOfStorable
    statsOf = mkStats

instance Sizeable Int64  where
    dataOf  = dataOfStorable
    statsOf = mkStats


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

dataOfWord8 :: Bytes
dataOfWord8
    = dataOf (undefined ::Word8)

dataOfWord16 :: Bytes
dataOfWord16
    = dataOf (undefined::Word16)

dataOfWord32 :: Bytes
dataOfWord32
    = dataOf (undefined::Word32)

-- --------------------

instance ( Sizeable t1, Sizeable t2
         , Typeable t1, Typeable t2
         ) =>
    Sizeable (t1, t2) where

    dataOf (_x1, _x2)
        = 2 .*. dataOfPtr

    statsOf xs@(x1, x2)
        = mkStats xs
          <>
          statsOf x1 <> statsOf x2

-- --------------------

instance ( Sizeable t1, Sizeable t2, Sizeable t3
         , Typeable t1, Typeable t2, Typeable t3
         ) =>
         Sizeable (t1, t2, t3) where

    dataOf (_x1, _x2, _x3)
        = 3 .*. dataOfPtr

    statsOf xs@(x1, x2, x3)
        = mkStats xs
          <>
          statsOf x1 <> statsOf x2 <> statsOf x3

-- --------------------

instance ( Sizeable t1, Sizeable t2, Sizeable t3, Sizeable t4
         , Typeable t1, Typeable t2, Typeable t3, Typeable t4
         ) =>
    Sizeable (t1, t2, t3, t4) where

    dataOf (_x1, _x2, _x3, _x4)
        = 4 .*. dataOfPtr

    statsOf xs@(x1, x2, x3, x4)
        = mkStats xs
          <>
          statsOf x1 <> statsOf x2 <> statsOf x3 <> statsOf x4

-- --------------------

instance (Sizeable t, Typeable t) => Sizeable (Maybe t) where
    dataOf x
        = case x of
            Just _  -> dataOfPtr
            Nothing -> dataOfSingleton

    statsOf x
        = case x of
            Just x1 -> constrStats "Just"    x <> statsOf x1
            Nothing -> constrStats "Nothing" x

-- --------------------

instance ( Sizeable t1, Sizeable t2
         , Typeable t1, Typeable t2
         ) =>
    Sizeable (Either t1 t2) where

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

instance (Sizeable a, Typeable a) => Sizeable [a] where
    dataOf xs
        = length xs .*. (dataOfConstr <> (2 .*. dataOfPtr))

    bytesOf             -- Lists are handled as a single object,
        = dataOf        -- all space is already accumulated in dataOf

    statsOf xs
        | null xs
            = mkStats xs

        | isGHCPrim (typeName hd)
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
          isGHCPrim n
              = "GHC.Types." `L.isPrefixOf` n
                &&
                drop 10 n `elem` ["Char", "Int", "Double", "Float", "Bool"]

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

instance (Sizeable v, Typeable v) => Sizeable (IM.IntMap v) where
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

instance ( Sizeable k, Sizeable v
         , Typeable k, Typeable v
         ) =>
    Sizeable (M.Map k v) where

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
    dataOf (BS.PS _payload _offset len)
        = (8 .*. dataOfPtr)                             -- 8 words for length field and pointers
          <> (wordAlign $                               -- size of byte sequence
              len .*. dataOfWord8
             )

    statsOf x@(BS.PS _payload offset len)
        = st3
          where
            tn  = typeName x
            st1 = mkStats  x                            -- extra statistics for real payload
                                                        -- and overhead by unused prefixes
            st2 =     addPart tn "<chars>"   (mkSize $    len .*. dataOfWord8) st1
            st3 | offset == 0
                    = st2
                | otherwise
                    = addPart tn "<offsets>" (mkSize $ offset .*. dataOfWord8) st2

instance Sizeable SS.ShortByteString where
    dataOf bs
        = (3 .*. dataOfPtr)                             -- 3 words for length field and pointers
          <> (wordAlign $                               -- size of byte sequence
              SS.length bs .*. dataOf (undefined ::Word8)
             )

    statsOf x
        = st2
          where
            tn  = typeName x
            st1 = mkStats  x
            st2 = addPart tn "<chars>" (mkSize $ SS.length x .*. dataOfWord8) st1

-- --------------------

{-
  data BL.ByteString = Empty
                     | Chunk {-# UNPACK #-} ! BS.ByteString ByteString
-}

instance Sizeable BL.ByteString where
    dataOf (BL.Empty)
        = dataOfSingleton

    dataOf (BL.Chunk _c _r)
        = 1 .*. dataOfPtr
          -- c is not counted here, because it's counted in statsOf c
          -- but it's an unpacked field (a bit tricky)

    statsOf x
       = case x of
            (BL.Empty    ) -> constrStats "Empty" x
            (BL.Chunk c r) -> constrStats "Chunk" x <> statsOf c <> statsOf r

-- ------------------------------------------------------------

-- the overhead of 8 words + constructor word is copied from
-- ByteString. Precise figures not yet found, except the issue,
-- that Text is the same as a ByteString with Word16 instead of Word8
-- for the real data

instance Sizeable T.Text where
    dataOf (T.Text _payload _offset len)
        = (8 .*. dataOfPtr)                             -- 8 words for length field and pointers
          <> (wordAlign $                               -- size of Word16 sequence
              len .*. dataOfWord16
             )

    statsOf x@(T.Text _payload offset len)
        = st3
          where
            tn  = typeName x
            st1 = mkStats x                             -- extra statistics for real payload
                                                        -- and overhead by unused prefixes
            st2 =     addPart tn "<chars>"   (mkSize $    len .*. dataOfWord16) st1
            st3 | offset == 0
                    = st2
                | otherwise
                    = addPart tn "<offsets>" (mkSize $ offset .*. dataOfWord16) st2

instance Sizeable TL.Text where
    dataOf (TL.Empty )
        = dataOfSingleton

    dataOf (TL.Chunk _c _r)
        = 1 .*. dataOfPtr
          -- c is not counted here, because it's counted in statsOf c
          -- but it's an unpacked field (a bit tricky)

    statsOf x
        = case x of
            (TL.Empty    ) -> constrStats "Empty" x
            (TL.Chunk c r) -> constrStats "Chunk" x <> statsOf c <> statsOf r

-- ------------------------------------------------------------
