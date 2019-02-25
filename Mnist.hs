{-# LANGUAGE FlexibleContexts, TypeOperators #-}
module Mnist ( downloadMnist
             , loadTrain
             , loadTest
               --
             , draw
             , imageAt
             , labelAt
             ) where

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
import Data.Word
import Network.HTTP.Simple ( parseRequest
                           , httpLBS
                           , getResponseBody
                           )
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist)

type DataSet = (Matrix Word8, Matrix Word8)
type Matrix a = R.Array R.U R.DIM2 a
type Vector a = R.Array R.U (R.Z R.:. Int) a

baseURL = "http://yann.lecun.com/exdb/mnist"
assetsDir = "assets"
keyFiles = [ ("train_image", "train-images-idx3-ubyte.gz")
           , ("train_label", "train-labels-idx1-ubyte.gz")
           , ("test_image", "t10k-images-idx3-ubyte.gz")
           , ("test_label", "t10k-labels-idx1-ubyte.gz")
           ]

mkURL f = baseURL ++ "/" ++ f
mkPath f = assetsDir ++ "/" ++ f

mkAssetsDir = do
  b <- doesDirectoryExist assetsDir
  when (not b) $ do
    putStrLn $ "Not found " ++ assetsDir ++ "."
    putStrLn "Creating ..."
    createDirectoryIfMissing True assetsDir

download :: FilePath -> IO ()
download f = do
  mkAssetsDir
  b <- doesFileExist $ mkPath f
  if b
    then putStrLn $ "Skip downloaing " ++ mkPath f ++" because it's already downloaded."
    else do
    putStr $ mkPath f ++ "Downloading... "
    r <- httpLBS =<< mkReq f
    BL.writeFile (mkPath f) $ getResponseBody r
    putStrLn "Done."
  where
    mkReq = parseRequest . mkURL

downloadMnist = do
  mkAssetsDir
  forM_ keyFiles $ \(_, f) -> do
    download f
  
toWord8List :: BL.ByteString -> [Word8]
toWord8List = map (read . show . fromEnum) . BL.unpack

toInt :: Integral a => BL.ByteString -> a
toInt = foldl' (\b a -> b * 256 + fromIntegral a) 0 . toWord8List

mnistImage = 2051
mnistLabel = 2049

load :: FilePath -> IO (Matrix Word8)
load f = do
  download f
  bs <- fmap GZ.decompress (BL.readFile $ mkPath f)
  let (typ,  r) = toInt *** id $ BL.splitAt 4 bs
  ret <- if typ == mnistImage
         then do { putStr $ "Loading image ... " ++ mkPath f
                 ; loadImage r
                 }
         else if typ == mnistLabel
              then do { putStr $ "Loading label ... " ++ mkPath f
                      ; loadLabel r
                      }
              else error $ "Unknown format " ++ show typ
  putStrLn " Done."
  return ret

loadImage :: BL.ByteString -> IO (Matrix Word8)
loadImage bs = do
  let (cnt, r) = toInt *** id $ BL.splitAt 4 bs
      (w,  r') = toInt *** id $ BL.splitAt 4 r
      (h, r'') = toInt *** id $ BL.splitAt 4 r'
      sz = w * h
  return $ R.fromListUnboxed (R.Z R.:. cnt R.:. sz) $ toWord8List r''

loadLabel :: BL.ByteString -> IO (Matrix Word8)
loadLabel bs = do
  let (cnt, r) = toInt *** id $ BL.splitAt 4 bs
  return $ R.fromListUnboxed (R.Z R.:. cnt R.:. 1) $ toWord8List r

imageAt :: Matrix Word8 -> Int -> Vector Word8
imageAt imgs i = R.computeUnboxedS $ R.slice imgs (R.Any R.:. i R.:. R.All)

labelAt :: Matrix Word8 -> Int -> Vector Word8
labelAt lbls i = R.computeUnboxedS $ R.slice lbls (R.Any R.:. i R.:. R.All)

drawAA :: Vector Word8 -> IO ()
drawAA xs = forM_ (toMatrix $ R.toList xs) prLn
  where
    toMatrix = unfoldr (bool <$> (Just . (splitAt 28)) <*> (const Nothing) <*> null)
    prLn ln = forM_ ln prCol >> putStrLn ""
    prCol d | d == 0 = putChar '.'
            | otherwise = putChar '#'

draw :: DataSet -> Int -> IO ()
draw ds i = do
  let (img, lbl) = (`imageAt` i) *** (`labelAt` i) $ ds
  putStrLn $ "Sample " ++ show i
  drawAA img
  putStrLn $ "Answer " ++ show (lbl R.! (R.Z R.:.0))

loadWith :: (FilePath, FilePath) -> IO (Matrix Word8, Matrix Word8)
loadWith (imgFile, lblFile) = do
  xi <- load imgFile
  xl <- load lblFile
  putStrLn "Displaying the first sample."
  draw (xi, xl) 0
  putStrLn "Done."
  return (xi, xl)

loadTrain :: IO (Matrix Word8, Matrix Word8)
loadTrain = loadWith ("train-images-idx3-ubyte.gz", "train-labels-idx1-ubyte.gz")
loadTest :: IO (Matrix Word8, Matrix Word8)
loadTest  = loadWith ("t10k-images-idx3-ubyte.gz", "t10k-labels-idx1-ubyte.gz")
