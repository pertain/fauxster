{-# LANGUAGE DeriveGeneric #-}

module Icd where

import GHC.Generics
import Data.Aeson
import Data.Time
import System.Random
import Data.List (sortBy)
import Data.Function (on)
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

data Device = Device
    { latitude       :: Float
    , longitude      :: Float
    , enable_log     :: Bool
    , enable_cache   :: Bool
    , purge_cache    :: Bool
    , enable_verbose :: Bool
    , verbosity      :: Int
    , enable_retry   :: Bool
    , retries        :: Int
    } deriving (Generic, Show)

instance ToJSON Device where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Device

instance Random Device where
    randomR
        ( Device _ _ _ _ _ _ _ _ _
        , Device _ _ _ _ _ _ _ _ _
        ) = random
    random g =
        let (lt, g1) = randomR (-90 :: Float, 90) g     -- latitude
            (lg, g2) = randomR (-180 :: Float, 180) g1  -- longitude
            (el, g3) = randomR (True, False) g2         -- enable_log
            (ec, g4) = randomR (True, False) g3         -- enable_cache
            (pc, g5) = randomR (True, False) g4         -- purge_cache
            (ev, g6) = randomR (True, False) g5         -- enable_verbose
            (v, g7)  = randomR (0 :: Int, 100) g6       -- verbosity
            (er, g8) = randomR (True, False) g7         -- enable_retry
            (r, g9)  = randomR (0 :: Int, 100) g8       -- retries
        in (Device lt lg el ec pc ev v er r, g9)


data Details = Details
    { mac_id       :: T.Text
    , manufacturer :: T.Text
    , model        :: T.Text
    , device_info  :: Device
    , bit_map      :: [Int]
    , generation   :: Int
    } deriving (Generic, Show)

instance ToJSON Details where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Details

instance Random Details where
    randomR
        ( Details _ _ _ _ _ _
        , Details _ _ _ _ _ _
        ) = random
    random g =
        let (mac, g1) = genMac g                    -- mac_id
            man       = T.pack "LG"                 -- manufacturer
            mdl       = T.pack "14FZ"               -- model
            (inf, g2) = random g1                   -- device_info
            (bmp, g3) = genBitMap g2                -- bit_map
            (gn, g4)  = randomR (1 :: Int, 99) g3   -- generation
        in (Details mac man mdl inf bmp gn, g4)


data Icd = Icd
    { event     :: T.Text
    , timestamp :: T.Text
    , _data     :: Details
    } deriving (Generic, Show)

instance ToJSON Icd where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Icd

instance Random Icd where
    randomR
        ( Icd _ _ _
        , Icd _ _ _
        ) = random
    random g =
        let xs       = mconcat . replicate 5 $
                           ["NEW", "UPDATE", "DELETE"]
            (es, g1) = shuffle xs g
            e        = T.pack $ head es     -- event
            (t, g2)  = genTimestamp g1      -- timestamp
            (d, g3)  = random g2            -- data
        in (Icd e t d, g3)


genBitMap :: RandomGen gen => gen -> ([Int], gen)
genBitMap g =
    let (len, g1) = randomR (1 :: Int, 20) g
        (raw, g2) = shuffle [1..20] g1
    in (take len raw, g2)

genMac :: RandomGen gen => gen -> (T.Text, gen)
genMac g =
    let xs        = mconcat . replicate 2 $
                        ['0'..'9'] <> ['A'..'F']
        (raw, g1) = shuffle (take 24 xs) g  -- 12-digit MAC
        --(raw, g1) = shuffle (take 12 xs) g  -- 6-digit MAC
    in (T.intercalate (T.pack ":") (T.chunksOf 2 $ T.pack raw), g1)

shuffle :: RandomGen gen => [a] -> gen -> ([a], gen)
shuffle xs g =
    let (g1, _) = split g
        ys      = take (length xs) $ randomRs (1 :: Int, 100000) g
    in (map fst $ sortBy (compare `on` snd) (zip xs ys), g1)

genTimestamp :: RandomGen gen => gen -> (T.Text, gen)
genTimestamp g =
    case fromGregorianValid yr mo dy of                         -- Type: Day
        Just d  ->
            let ut  = UTCTime d (realToFrac sc)                 -- Type: UTCTime
                zt  = utcToZonedTime (hoursToTimeZone zo) ut    -- Type: ZonedTime
                raw = T.pack $ formatTime defaultTimeLocale "%FT%X%03Q%z" zt
            in (T.dropEnd 2 raw <> T.pack ":00", g5)
        Nothing -> genTimestamp g2
    where
        (yr, g1) = randomR (1999, 2018) g           -- year
        (mo, g2) = randomR (1, 12) g1               -- month
        (dy, g3) = randomR (1, 31) g2               -- day
        (sc, g4) = randomR (0 :: Double, 86399) g3  -- seconds since midnight
        (zo, g5) = randomR (-12, 12) g4             -- time zone offset (from UTC)

genRecords :: RandomGen gen => gen -> [B.ByteString]
genRecords g = B.concat . BL.toChunks <$> recs
    where
        icds = randoms g :: [Icd]
        recs = encode <$> icds

kinesisFormat :: [B.ByteString] -> [(B.ByteString, T.Text)]
kinesisFormat = flip zip $ indices
    where
        indices = T.pack . show <$> [1..]
