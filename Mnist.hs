{-# LANGUAGE FlexibleContexts #-}
module Mnist where

import GHC.Int
import Control.Arrow ((&&&),(***))
import Control.Monad
import qualified Codec.Compression.GZip as GZ ( compress
                                              , decompress
                                              )
import qualified Data.Array.Repa as R
import qualified Data.ByteString.Lazy as BL
import Data.List (foldl')
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

mkURL f = baseURL ++ "/" ++ f
mkPath f = assetsDir ++ "/" ++ f

download f = do
  r <- httpLBS =<< mkReq f
  BL.writeFile (mkPath f) $ getResponseBody r
  where
    mkReq = parseRequest . mkURL

downloadMnist = do
  forM_ keyFiles $ \(_, f) -> do
    download f

toDoubleList :: BL.ByteString -> [Double]
toDoubleList = map (read . show . fromEnum) . BL.unpack

toInt :: Integral a => BL.ByteString -> a
toInt = round . foldl' (\b a -> b * 256 + a) 0 . toDoubleList

load :: String -> IO ((R.Array R.U R.DIM2) Double)
load f = do
    bs <- fmap GZ.decompress (BL.readFile $ mkPath f)
    let (typ,  r) = toInt *** id $ BL.splitAt 4 bs
    if typ == 2051
      then loadImage r
      else if typ == 2049
      then loadLabel r
      else error $ "Unknown format " ++ show typ

loadImage :: BL.ByteString -> IO ((R.Array R.U R.DIM2) Double)
loadImage bs = do
  let (cnt, r) = toInt *** id $ BL.splitAt 4 bs
      (w,  r') = toInt *** id $ BL.splitAt 4 r
      (h, r'') = toInt *** id $ BL.splitAt 4 r'
      sz = w * h
  return $ R.fromListUnboxed (R.Z R.:. cnt R.:. sz) $ toDoubleList r''

loadLabel :: BL.ByteString -> IO ((R.Array R.U R.DIM2) Double)
loadLabel bs = do
  let (cnt, r) = toInt *** id $ BL.splitAt 4 bs
  return $ R.fromListUnboxed (R.Z R.:. cnt R.:. 1) $ toDoubleList r

main = do
  createDirectoryIfMissing True assetsDir
  downloadMnist
