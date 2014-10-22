{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as B

import Control.Concurrent.MVar

import Numeric (showHex)
import qualified Crypto.Hash.SHA1 as SHA1

import qualified Data.List as L

import System.Environment (getArgs)

import Data.Aeson
import Blaze.ByteString.Builder (fromLazyByteString)
import Network.HTTP.Types (status200)
import Network.Wai
import Network.Wai.Handler.Warp (run)

import Rawhide

prettyHash = concat . map (\h -> let s = showHex h "" in if length s == 1 then "0" ++ s else s) . SB.unpack

application min max m z c store request respond = do
  existing <- takeMVar store
  body <- lazyRequestBody request
  let chunks = chunk min max m z c body
  let pretty = map (\c -> prettyHash (SHA1.hashlazy c) ++ "-" ++ show (B.length c)) chunks
  let union = L.union existing pretty
  let intersect = L.intersect existing pretty
  putMVar store union
  let overlap = fromIntegral (L.length intersect) / fromIntegral (L.length pretty)
  let result = object [("chunks", toJSON pretty),("overlap", toJSON (overlap::Double))]
  respond $ responseBuilder
    status200
    [("Content-Type", "text/plain")]
    (fromLazyByteString (encode result))

main = do
  [min, max, m, z] <- getArgs
  let min' = read min
  let max' = read max
  let m' = read m
  let z' = read z
  let c = (0,0)
  store <- newMVar []
  run 3000 $ application min' max' m' z' c store
