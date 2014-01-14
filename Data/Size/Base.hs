{-# LANGUAGE DefaultSignatures #-}

module Data.Size.Base
    ( Bytes
    , Size
    , SizeTable
    , SizeStatistics
    , Sizeable(..)

    , bitsPerWord
    , bytesPerWord
    , bytesPerChar
    , bytesToWords
    , wordAlign
    , mkBytes
    , mkSize
    , dataSize
    , singletonSize
    , dataOfObj
    , dataOfConstr
    , dataOfPtr
    , dataOfSingleton

    , (.*.)
    , typeName
    , setName
    , addSize
    , addPart
    , mkObject
    , mkStats
    , mkStats'
    , constrStats
    , constrStats'
    , showStats

    , mempty            -- re-exports of Monoid
    , mappend
    , mconcat
    , (<>)
    )
where
import           Control.DeepSeq

import qualified Data.List        as L
import qualified Data.Map.Strict  as M
import           Data.Monoid
import           Data.Typeable
import qualified Foreign.Storable as FS

-- ----------------------------------------

infix  7 .*.

-- --------------------

bytesPerWord :: Int
bytesPerWord = bitsPerWord `div` 8

bytesPerChar :: Int
bytesPerChar = FS.sizeOf ' '

bitsPerWord :: Int
bitsPerWord = cnt 1 $ iterate (*2) (1::Int)
    where
      cnt i (x : xs)
          | x < 0 = i
          | otherwise = cnt (i+1) xs
      cnt _ _                           -- just to turn of pattern match warning
          = undefined

bytesToWords :: Int -> Int
bytesToWords i = (i + bytesPerWord - 1) `div` bytesPerWord

-- --------------------

data Bytes
    = Bytes
      { _bytes :: ! Int
      , _align :: ! Int
      }
    deriving (Eq, Show)

instance Monoid Bytes where
    mempty
        = Bytes 0 1
    (Bytes bs1 al1) `mappend` (Bytes bs2 al2)
        = Bytes (align bs1 al2 + bs2) (al1 `max` al2)

instance Scale Bytes where
    i .*. (Bytes bs al)
        = Bytes (i * bs) al

align :: Int -> Int -> Int
align x a
    = (x + (a - 1)) `div` a * a

mkBytes :: Int -> Int -> Bytes
mkBytes bs' al'
    = Bytes (align bs al) al
      where
        bs = bs' `max` 0
        al = al' `max` 1

wordAlign :: Bytes -> Bytes
wordAlign x
    = x <> Bytes 0 bytesPerWord

dataOfSingleton :: Bytes
dataOfSingleton
    = Bytes 0 bytesPerWord

dataOfConstr :: Bytes
dataOfConstr
    = Bytes bytesPerWord bytesPerWord

dataOfPtr :: Bytes
dataOfPtr
    = Bytes bytesPerWord bytesPerWord

dataOfObj :: Bytes -> Bytes
dataOfObj w@(Bytes bs _al)
    | bs == 0     = w                           -- singleton
    | otherwise   = wordAlign $ dataOfConstr <> w

-- --------------------

-- | Counter for # of objects and # of words

data Size
    = Size
      { _objCnt  :: {-# UNPACK #-} ! Int
      , _byteCnt :: {-# UNPACK #-} ! Int
      }
    deriving (Eq, Show)

-- | Make a counter for one object with n fields
--
-- constructors count 1 word
-- If # words for the data is 0, it's a singleton,
-- so the # of objects are not accumulated

mkSize :: Bytes -> Size
mkSize (Bytes n _)
    | n == 0    = singletonSize
    | otherwise = Size 1 n

-- get the # of bytes for the data fields of a value, without counting the constructor field

dataSize :: Size -> Int
dataSize (Size _o w) = (w - bytesPerWord) `max` 0  -- decrement constructor size

-- | The size value of a singleton
--
-- Singletons are only counted once

singletonSize :: Size
singletonSize = Size 1 0

instance Monoid Size where
    mempty
        = singletonSize         -- Size 0 0
    mappend c1@(Size o1 w1) c2@(Size o2 w2)
        | c1 == singletonSize = c2      -- singletons don't accumulate
        | c2 == singletonSize = c1      --     "        "       "
        | otherwise           = Size (o1 + o2) (w1 + w2)
    mconcat
        = L.foldl' mappend mempty

instance Scale Size where
    i .*. c@(Size o w)
        | c == singletonSize = c
        | otherwise          = Size (i*o) (i*w)

-- --------------------

newtype SizeTable
    = ST (M.Map (String, String) Size)
      deriving (Show)

instance Monoid SizeTable where
    mempty = ST $! M.empty
    mappend (ST t1) (ST t2)
        = ST $! M.unionWith (<>) t1 t2
    mconcat
        = L.foldl' mappend mempty

instance Scale SizeTable where
    i .*. (ST t)
        | i == 0    = mempty
        | otherwise = ST $! M.map (i .*.) t

-- --------------------

data SizeStatistics
    = SST
      { _nameof :: ! String
      , _accu   :: ! Size
      , _parts  :: ! SizeTable
      }
--    deriving Show

mkSST :: String -> Size -> SizeTable -> SizeStatistics
mkSST n c t
    = rnf n `seq` SST n c t

instance Show SizeStatistics where show = showStats

instance Monoid SizeStatistics where
    mempty = mkSST "" mempty mempty
    mappend (SST n1 c1 t1) (SST n2 c2 t2)
        = mkSST n c t
          where
            n = if null n1 then n2 else n1
            c = c1 <> c2
            t = t1 <> t2
    mconcat
        = L.foldl' mappend mempty

instance Scale  SizeStatistics where
    i .*. (SST n c t)
        = mkSST n (i .*. c) (i .*. t)

-- --------------------

class Monoid a => Scale a where
    (.*.) :: Int -> a -> a

-- --------------------

class Sizeable a where
    dataOf     :: a -> Bytes
    bytesOf    :: a -> Bytes
    objectsOf  :: a -> Size
    statsOf    :: a -> SizeStatistics

    bytesOf    = dataOfObj . dataOf
    objectsOf  = _accu . statsOf
    statsOf    = mkStats' (const "<type with no name>")

mkStats :: (Typeable a, Sizeable a) => a -> SizeStatistics
mkStats = mkStats' typeName

typeName :: (Typeable a) => a -> String
typeName x
    = (tyConModule . fst . splitTyConApp $ t) ++ "." ++ show t
      where
        t = typeOf x

constrStats :: (Typeable a, Sizeable a) => String -> a -> SizeStatistics
constrStats cn x
    = constrStats' typeName cn x

mkStats' :: (Sizeable a) => (a -> String) -> a -> SizeStatistics
mkStats' f  = constrStats' f ""

constrStats' :: (Sizeable a) => (a -> String) -> String -> a -> SizeStatistics
constrStats' nameOf cn x
    = st3
    where
      nm  = nameOf x
      cnt = mkObject x
      st1 = addSize cnt $ setName nm $ mempty   -- add the stats for the datatype
      st2 = addPart nm "" cnt st1
      st3 | null cn   =                   st2
          | otherwise = addPart nm cn cnt st2   -- add the stats for the constructor

-- ------------------------------------------------------------

insertSizeTable :: (String, String) -> Size -> SizeTable -> SizeTable
insertSizeTable tn v (ST t) = ST $ M.insertWith (<>) tn v t

setName :: String -> SizeStatistics -> SizeStatistics
setName n st
    = st { _nameof = n }

addSize :: Size -> SizeStatistics -> SizeStatistics
addSize c st
    = st { _accu = c <> _accu st }

addPart :: String -> String -> Size ->  SizeStatistics -> SizeStatistics
addPart tn cn c st
    = st { _parts = insertSizeTable (tn, cn) c $ _parts st }

mkObject :: (Sizeable a) => a -> Size
mkObject x
    | n == 0    = singletonSize
    | otherwise = Size 1 n
    where
      (Bytes n _) = bytesOf x

-- ------------------------------------------------------------

showStats :: SizeStatistics -> String
showStats (SST name (Size oc bc) (ST parts))
    = unlines $
      header
      ++ "accumulated data:"
      :   toLine' ( (charToString name, "")
                  , (showNum oc, (showNum {- . bytesToWords -} $ bc, ""))
                  )
      : ""
      : "components:"
      :  toTable
      where
        statsTable = L.map toString . M.toList $ parts

        toString :: ((String, String), Size) -> ((String, String), (String, (String, String)))
        toString ((tn, cn), Size os bs)
            = ( ( if null cn
                  then charToString tn
                  else blankName
                , cn
                )
              , ( showNum os
                , ( showNum {- . bytesToWords -} $ bs
                  , showNum $ (bs + os - 1) `div` os
                  )
                )
              )
        toTable
            = L.map toLine' $ statsTable

        toLine' :: ((String, String), (String, (String, String))) -> String
        toLine' ((tn, cn), (os, (bs, sz)))
            = unwords [ if null cn
                        then expR widthCol1 tn
                        else blankName ++ expR widthCol1 cn
                      , expL widthCol2 os
                      , expL widthCol3 bs
                      , expL widthCol4 sz
                      ]

        blankName   = replicate 8 ' '

        widthCol1
            = 16 `max` length col1 `max` (L.maximum . L.map (uncurry width . fst) $ statsTable)
            where
              width tn cn
                  = length tn + length cn
        widthCol2
            = 16 `max` length col2 `max` (L.maximum . L.map (length . fst . snd) $ statsTable)

        widthCol3
            = 16 `max` length col3 `max` (L.maximum . L.map (length . fst . snd . snd) $ statsTable)

        widthCol4
            = 16 `max` length col4 `max` (L.maximum . L.map (length . snd . snd . snd) $ statsTable)

        charToString
            = subst "[Char]" "String"

        col1 = "type/constructor"
        col2 = "# objects"
        col3 = "# bytes" -- "# word" ++ show bitsPerWord
        col4 = "bytes/obj"
        header
            = [l1, l2, l3]
              where
                l1 = unwords [ expR widthCol1 col1
                             , expL widthCol2 col2
                             , expL widthCol3 col3
                             , expL widthCol4 col4
                             ]
                l2 = map (const '=') l1
                l3 = ""

expL :: Int -> String -> String
expL n s
    | n < n'    = s
    | otherwise = reverse . take n . reverse . (replicate n ' ' ++) $ s
    where
      n' = length s

expR :: Int -> String -> String
expR n s
    | n < n'    = s
    | otherwise = take n $ s ++ replicate n ' '
    where
      n' = length s

showNum :: Int -> String
showNum
    = reverse . insComma . reverse . show
    where
      insComma (x1 : x2 : x3 : xs4@(_ : _))
          = x1 : x2 : x3 : ',' : insComma xs4
      insComma xs
          = xs

subst :: String -> String -> String -> String
subst _ _ []
    = []
subst xs ys inp
    | L.isPrefixOf xs inp
        = ys ++ subst xs ys (L.drop (length xs) inp)
    | otherwise
        = head inp : subst xs ys (tail inp)

-- ------------------------------------------------------------
