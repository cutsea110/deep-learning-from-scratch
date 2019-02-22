module Mnist where

import Control.Monad
import qualified Codec.Compression.GZip as GZ ( compress
                                              , decompress
                                              )
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Simple ( parseRequest
                           , httpLBS
                           , getResponseBody
                           )
import System.Directory (createDirectoryIfMissing)

baseURL = "http://yann.lecun.com/exdb/mnist"
assetsDir = "assets"
keyFiles = [ ("train_image", "train-images-idx3-ubyte.gz")
           , ("train_label", "train-labels-idx1-ubyte.gz")
           , ("test_image", "t10k-images-idx3-ubyte.gz")
           , ("test_label", "t10k-labels-idx1-ubyte.gz")
           ]

download f = do
  let savePath = assetsDir ++ "/" ++ f
  r <- httpLBS =<< mkReq f
  BL.writeFile savePath $ getResponseBody r
  where
    mkReq f = parseRequest $ baseURL ++ "/" ++ f

downloadMnist = do
  forM_ keyFiles $ \(_, f) -> do
    download f

main = do
  createDirectoryIfMissing True assetsDir
  downloadMnist
  
