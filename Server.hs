{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as B

import Control.Concurrent.MVar

import Numeric (showHex)
import qualified Crypto.Hash.SHA1 as SHA1

import qualified Data.List as L
import qualified Data.Map as Map

import System.Environment (getArgs)

import Data.Aeson
import Blaze.ByteString.Builder (fromLazyByteString)
import Network.HTTP.Types (status200)
import Network.Wai
import Network.Wai.Handler.Warp (run)

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Default.Class
import Control.Lens

import Statistics.Sample hiding (range)
import Statistics.Sample.Histogram
import qualified Data.Vector as Vector

import Rawhide

prettyHash = concat . map (\h -> let s = showHex h "" in if length s == 1 then "0" ++ s else s) . SB.unpack

plot :: Map.Map String Int -> Renderable ()
plot chunks = toRenderable layout
  where
    layout = layoutlr_title .~ "Rawhide Chunks"
           $ layoutlr_plots .~ [Right histogram, Left points, Left meanSize, Left meanOverlap]
           $ layoutlr_left_axis . laxis_generate .~ scaledAxis def (0.0,4.0)
           $ def
    meanSize = vlinePlot "mean size" (def & line_dashes .~ [4]) $ LogValue $ getMeanSize chunks
    meanOverlap = hlinePlot "mean overlap" (def & line_dashes .~ [4]) $ getMeanOverlap chunks
    points = toPlot $
      def & plot_points_title .~ "Size vs. Overlap"
          & plot_points_values .~ (getPoints chunks)
          & plot_points_style .~ (def & point_radius .~ 3)
    histogram = plotBars $
      def & plot_bars_values .~ (getSizeHistogram chunks)
          & plot_bars_alignment .~ BarsLeft

getSizeSample :: Map.Map String Int -> Vector.Vector Double
getSizeSample chunks = Vector.fromList $ map (\s -> read $ L.drop 41 s) (Map.keys chunks)

getOverlapSample :: Map.Map String Int -> Vector.Vector Double
getOverlapSample chunks = Vector.fromList $ map fromIntegral $ Map.elems chunks

getMeanSize :: Map.Map String Int -> Double
getMeanSize chunks = mean $ getSizeSample chunks

getMeanOverlap :: Map.Map String Int -> Double
getMeanOverlap chunks = mean $ getOverlapSample chunks

getSizeHistogram :: Map.Map String Int -> [(LogValue, [Double])]
getSizeHistogram chunks = L.zip (map LogValue $ Vector.toList xs) (map (\y -> [fromIntegral y]) $ Vector.toList ys)
  where dat = Vector.fromList $ map (\s -> read $ L.drop 41 s) (Map.keys chunks)
        (xs, ys) = histogram 50 dat

getPoints :: Map.Map String Int -> [(LogValue, Double)]
getPoints chunks = map (\(s,i) -> (LogValue $ read $ L.drop 41 s, fromIntegral i)) (Map.assocs chunks)

application min max m z c store request respond = do
  existing <- takeMVar store
  body <- lazyRequestBody request
  let chunks = chunk min max m z c body
  let pretty = Map.fromList $ map (\c -> (prettyHash (SHA1.hashlazy c) ++ "-" ++ show (B.length c), 1)) chunks
  let sizes = map (\c -> B.length c) chunks
  let union = Map.unionWith (+) existing pretty
  let intersect = Map.intersection existing pretty
  putMVar store union
  let overlap = fromIntegral (Map.size intersect) / fromIntegral (Map.size pretty)
  let result = object [("chunks", toJSON (Map.keys pretty)),("overlap", toJSON (overlap::Double))]
  renderableToFile def "chart.png" $ plot union
  respond $ responseBuilder
    status200
    [("Content-Type", "text/plain")]
    (fromLazyByteString (encode result))

-- 4288 4194303 71 2048
-- 16288 4194303 143 2078
-- 36288 4194303 71 778
-- 64288 4194303 143 2078

main = do
  [min, max, m, z] <- getArgs
  let min' = read min
  let max' = read max
  let m' = read m
  let z' = read z
  let c = (0,0)
  store <- newMVar Map.empty
  run 3000 $ application min' max' m' z' c store
