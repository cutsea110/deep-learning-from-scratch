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
files = [ ("train_image", "train-images-idx3-ubyte.gz")
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

main = do
  createDirectoryIfMissing True assetsDir
  forM_ files $ \(_, f) -> do
    download f
