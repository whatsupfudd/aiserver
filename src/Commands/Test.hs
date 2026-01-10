module Commands.Test where

import Control.Exception (bracket)
import Control.Monad.Cont (runContT, ContT (..))    -- liftIO

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import System.Posix (installHandler)

import Network.Wai.Handler.Warp as Wrp

import qualified Assets.Types as S3
import qualified Assets.S3Ops as S3
import Options.Runtime as Rto

testCmd :: Text -> Rto.RunOptions -> IO ()
testCmd subCmd rtOpts = do
  putStrLn "@[serverCmd] going to start mainAction."
  case rtOpts.s3store of
    Nothing -> putStrLn $ "@[testCmd] no s3store configured."
    Just s3store ->
      let
        s3Conn = S3.makeS3Conn s3store
      in do
      putStrLn $ "@[testCmd] s3store: " <> show s3store
      -- fetchObject s3Conn subCmd
      -- showPathList s3Conn subCmd
      showDirectory s3Conn subCmd Nothing


showDirectory :: S3.S3Conn -> Text -> Maybe Text -> IO ()
showDirectory s3conn subCmd mbPrefix =
  let
    prefix = fromMaybe subCmd mbPrefix
  in do
  rezA <- S3.listFiles s3conn Nothing
  case rezA of
    Left err ->
      putStrLn $ "@[testCmd] listFiles err: " <> show err
    Right aRez -> do
      putStrLn $ "@[testCmd] list of " <> (T.unpack prefix) <> " length: " <> (show . length $ aRez)
      putStrLn $ "@[testCmd] list: " <> show aRez
