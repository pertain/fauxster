module Main where

import System.Random
import qualified Data.Text as T
import Icd
import AwsTools

main :: IO ()
main = do
    g <- newStdGen

    let role = T.pack "<role-arn>"
        stream = T.pack "<stream-name>"
        rawData = genRecords g
        prrData = groupRequests $ kinesisFormat rawData

    -- Add Bulk Records
    -- ----------------
    -- The integer passed here represents the number of
    -- putRecords requests, each of which contains 500 records
    addRecordsRequests 250 role stream prrData

    -- Print some sample data
    --mapM_ print $ take 10 rawData
