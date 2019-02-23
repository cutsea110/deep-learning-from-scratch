{-# LANGUAGE FlexibleContexts, TypeOperators #-}
module Mnist where

import GHC.Int
import Control.Arrow ((&&&),(***))
import Control.Monad
import qualified Codec.Compression.GZip as GZ ( compress
                                              , decompress
                                              )
import qualified Data.Array.Repa as R
import qualified Data.ByteString.Lazy as BL
import Data.Bool (bool)
import Data.List (foldl', unfoldr)
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
  putStr "Downloading... "
  forM_ keyFiles $ \(_, f) -> do
    download f
  putStrLn "Done."
  
toDoubleList :: BL.ByteString -> [Double]
toDoubleList = map (read . show . fromEnum) . BL.unpack

toInt :: Integral a => BL.ByteString -> a
toInt = round . foldl' (\b a -> b * 256 + a) 0 . toDoubleList

mnistImage = 2051
mnistLabel = 2049

load :: String -> IO ((R.Array R.U R.DIM2) Double)
load f = do
    bs <- fmap GZ.decompress (BL.readFile $ mkPath f)
    let (typ,  r) = toInt *** id $ BL.splitAt 4 bs
    if typ == mnistImage
      then loadImage r
      else if typ == mnistLabel
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

imageAt :: R.Array R.U R.DIM2 Double -> Int -> R.Array R.U (R.Z R.:. Int) Double
imageAt imgs i = R.computeUnboxedS $ R.slice imgs (R.Any R.:. i R.:. R.All)

labelAt :: R.Array R.U R.DIM2 Double -> Int -> R.Array R.U (R.Z R.:. Int) Double
labelAt lbls i = R.computeUnboxedS $ R.slice lbls (R.Any R.:. i R.:. R.All)

drawAA :: R.Array R.U (R.Z R.:. Int) Double -> IO ()
drawAA xs = forM_ (toMatrix $ R.toList xs) prLn
  where
    toMatrix = unfoldr (bool <$> (Just . (splitAt 28)) <*> (const Nothing) <*> null)
    prLn ln = forM_ ln prCol >> putStrLn ""
    prCol d | d == 0 = putChar '.'
            | otherwise = putChar '#'

draw :: (R.Array R.U R.DIM2 Double, R.Array R.U R.DIM2 Double) -> Int -> IO ()
draw (lbls, imgs) i = do
  let (lbl, img) = (labelAt lbls i, imageAt imgs i)
  putStrLn $ "Sample " ++ show i
  drawAA img
  putStrLn $ "Answer " ++ show (round $ lbl R.! (R.Z R.:.0))

loadWith (lblFile, imgFile) = do
  putStr "Loading... "
  xl <- load lblFile
  xi <- load imgFile
  draw (xl, xi) 0
  putStrLn "Done."
  return (xl, xi)

loadTrain = loadWith ("train-labels-idx1-ubyte.gz", "train-images-idx3-ubyte.gz")
loadTest  = loadWith ("t10k-labels-idx1-ubyte.gz", "t10k-images-idx3-ubyte.gz")

main = do
  createDirectoryIfMissing True assetsDir
  downloadMnist

  (xl, xi) <- loadTrain
  (tl, ti) <- loadTest

  print "Done."
