{-# LANGUAGE BangPatterns #-}

module Data.Size.Base
    ( module Data.Size.Base
    , module Data.Monoid
    )
where

import qualified Data.List       as L
import qualified Data.Map.Strict as M
import           Data.Monoid
import           Data.Typeable

-- ----------------------------------------

infix  7 .*.

-- --------------------

bytesPerWord :: Int
bytesPerWord = bitsPerWord `div` 8

bitsPerWord :: Int
bitsPerWord = cnt 1 $ iterate (*2) (1::Int)
    where
      cnt i (x : xs)
          | x < 0 = i
          | otherwise = cnt (i+1) xs

-- --------------------

-- | Counter for # of objects and # of words

data Size
    = Size
      { _objCnt  :: ! Int
      , _wordCnt :: ! Int
      }
    deriving (Eq, Show)

-- | Make a counter for one object with n fields

mksize :: Int -> Size
mksize 0 = singletonSize
mksize n = Size 1 (n + 1)

-- | The size value of a singleton
--
-- Singletons are only counted once

singletonSize :: Size
singletonSize = Size 1 0

instance Monoid Size where
    mempty
        = Size 0 0
    mappend c1@(Size o1 w1) c2@(Size o2 w2)
        | c1 == singletonSize = c2	-- singletons don't accumulate
        | c2 == singletonSize = c1	--     "        "       "
        | otherwise           = Size (o1 + o2) (w1 + w2)

instance Scale Size where
    i .*. c@(Size o w)
        | c == singletonSize = c
        | otherwise          = Size (i*o) (i*w)

-- --------------------

newtype SizeTable
    = ST (M.Map String Size)
      deriving (Show)

instance Monoid SizeTable where
    mempty = ST M.empty
    mappend (ST t1) (ST t2)
        = ST $ M.unionWith (<>) t1 t2

instance Scale SizeTable where
    i .*. (ST t)
        | i == 0    = mempty
        | otherwise = ST $ M.map (i .*.) t

-- --------------------

data SizeStatistics
    = SST
      { _nameof :: ! String
      , _accu   :: ! Size
      , _parts  :: ! SizeTable
      }

instance Show SizeStatistics where
    show = showstats

instance Monoid SizeStatistics where
    mempty = SST "" mempty mempty
    mappend (SST n1 c1 t1) (SST n2 c2 t2)
        = SST n c t
          where
            n = if null n1 then n2 else n1
            c = c1 <>        c2
            t = t1 <> t2

instance Scale  SizeStatistics where
    i .*. (SST n c t)
        = SST n (i .*. c) (i .*. t)

-- --------------------

class Monoid a => Scale a where
    (.*.) :: Int -> a -> a

-- --------------------

class Typeable a => Sizeable a where
    nameof    :: a -> String
    sizeof    :: a -> Size
    statsof   :: a -> SizeStatistics

    nameof x
        | m == "GHC.Types" = n
        | otherwise        = m ++ "." ++ n
        where
          t = fst . splitTyConApp . typeOf $ x
          m = tyConModule t
          n = tyConName t

    sizeof _  = mksize       1	-- defaults for primitive types
    statsof x = mkstats x "" 1	--     "     "      "       "

-- --------------------

nameof' :: Typeable a => a -> String
nameof' x
    | m == "GHC.Types" = n
    | otherwise        = m ++ "." ++ n
      where
        t = fst . splitTyConApp . typeOf $ x
        m = tyConModule t
        n = tyConName t

-- ------------------------------------------------------------

insertSizeTable :: String -> Size -> SizeTable -> SizeTable
insertSizeTable k v (ST t) = ST $ M.insertWith (<>) k v t

-- ------------------------------------------------------------

setName :: String -> SizeStatistics -> SizeStatistics
setName n st
    = st { _nameof = n }

addSize :: Size -> SizeStatistics -> SizeStatistics
addSize c st
    = st { _accu = c <> _accu st }

addPart :: String -> Size ->  SizeStatistics -> SizeStatistics
addPart n c st
    = st { _parts = insertSizeTable n c $ _parts st }

mkstats :: Sizeable a => a -> String -> Int -> SizeStatistics
mkstats x cn w
    = st3
    where
      n = nameof x
      cnt = Size 1 (if w == 0 then 0 else w + 1)	-- overhead for tagfield
      st1 = addSize cnt $ setName n $ mempty
      st2 = addPart n cnt st1
      st3 | null cn = st2
          | otherwise = addPart (n ++ " " ++ cn) cnt st2

showstats :: SizeStatistics -> String
showstats (SST name cnt (ST parts))
    = unlines $
      header
      ++ "total value:"
      :   toLine name cnt
      : ""
      : "components:"
      :  (L.map (uncurry toLine) . M.toList $ parts)
      where
        ! widthName = L.maximum . L.map length . ([col1, name] ++) . M.keys
                      $ parts
        ! widthObj  = 16 `max` length col2
        ! widthWord = 16 `max` length col3
        col1 = "type/constructor"
        col2 = "# object"
        col3 = "# word" ++ show bitsPerWord
        header
            = [l1, l2, l3]
              where
                l1 = unwords [ expR widthName col1
                             , expL widthObj  col2
                             , expL widthWord col3
                             ]
                l2 = map (const '=') l1
                l3 = ""

        toLine n (Size o w)
            = unwords [ indentConstr widthName   n
                      , expL         widthObj  $ show o
                      , expL         widthWord $ show w
                      ]
        indentConstr i n
            | length ws == 2 = replicate 8 ' ' ++ expR i (concat . drop 1 $ ws)
            | otherwise    =                      expR i n
            where
              ws = words n

        expL n s
            | n < n'    = s
            | otherwise = reverse . take n . reverse . (replicate n ' ' ++) $ s
            where
              n' = length s

        expR n s
            | n < n'	= s
            | otherwise = take n $ s ++ replicate n ' '
            where
              n' = length s

-- ------------------------------------------------------------
