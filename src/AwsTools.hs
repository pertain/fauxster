module AwsTools where

import Control.Monad.Trans.AWS
import Network.AWS.STS
import Network.AWS.Prelude
import Network.AWS.Kinesis
import Control.Lens
import System.Random
import Data.List.Split (chunksOf)
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE
import qualified Data.ByteString as B

{- Single record testing -}
addRecord :: Env -> B.ByteString -> IO ()
addRecord e rec = do
    g <- newStdGen

    -- TODO: add exception handling, response status
    let (x, g1) = randomR (1 :: Int, 10000) g
        pkey = T.pack "partitionkey-0" <> (T.pack . show) x
        snm = T.pack "<stream-name>"
        pr = putRecord snm rec pkey
    runResourceT . runAWST e . within NorthVirginia $ send pr
    return ()

{- Still need to work on this type signature
addRecords :: (Control.Monad.Catch.MonadCatch m,
               Control.Monad.IO.Unlift.MonadUnliftIO m) =>
               Env -> NonEmpty PutRecordsRequestEntry ->
               m PutRecordsResponse
-}
addRecords e stm rs =
    let prs = putRecords rs stm
    in runResourceT . runAWST e . within NorthVirginia $ send prs

assumeAwsRole :: Env -> T.Text -> IO Env
assumeAwsRole env rol = do
    let ses = T.pack "assumed_session_004"
        ar = assumeRole rol ses
    runResourceT . runAWST env . within NorthVirginia $ do
        arr <- send ar
        let arrc = view arrsCredentials arr
        case arrc of
            Nothing -> return env
            Just a  -> do
                let k = view accessKeyId a
                    s = view secretAccessKey a
                    mt = view sessionToken a
                case mt of
                    Nothing -> return env
                    Just t -> newEnv $ FromSession k s t

-- AWS Kinesis 'putRecords' is limited to 500 records per call
groupRequests :: [(B.ByteString, T.Text)]
              -> [NonEmpty PutRecordsRequestEntry]
groupRequests ps = NE.fromList <$> (chunksOf 500 prres)
    where
        prres = uncurry putRecordsRequestEntry <$> ps

addRecordsRequests :: Int
                   -> T.Text
                   -> T.Text
                   -> [NonEmpty PutRecordsRequestEntry]
                   -> IO ()
addRecordsRequests n r s xs = do
    env <- newEnv Discover
    role_env <- assumeAwsRole env r
    mapM_ (addRecords role_env s) (take n xs)
