module AdventOfCode.Day4 (
  lowestPositiveNumber
    ) where

import qualified Data.ByteString.Char8 as BS
import Crypto.Hash

md5 :: String -> Digest MD5
md5 = hash . BS.pack

digestToHexString :: Digest a -> String
digestToHexString = BS.unpack.digestToHexByteString

leadingZeroHashLength :: String -> Int
leadingZeroHashLength input = leadingZ 0 ((digestToHexString.md5) input)
  where
    leadingZ :: Int -> String -> Int
    leadingZ cur (x:xs)
      | x == '0'    = leadingZ (cur + 1) xs
      | otherwise   = cur
    leadingZ cur [] = cur

runSingleAttempt :: String -> Int -> Int
runSingleAttempt secretKey idx = leadingZeroHashLength (secretKey ++ (show idx))

lowestPositiveNumber :: Int -> String -> Int
lowestPositiveNumber leadingZeros secretKey = runAttempt 1
    where
      runAttempt idx = if runSingleAttempt secretKey idx >= leadingZeros then idx else runAttempt (idx + 1)
