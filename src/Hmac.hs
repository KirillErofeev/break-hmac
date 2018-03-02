module Hmac where

import Data.Bits
import Data.Word
import Data.ByteString as B (pack, append)
import Data.Numbers.Primes as Primes

import Control.Concurrent

import Crypto.Hash.SHA1 (hash)

-- 64bit
--key = take 64 . repeat $ (3 :: Word8)
key :: [Word8]
key = take 64 [1..]

msg = pack (take 64 primes :: [Word8])
--bs = 64
--os = 20
--
hmac key msg = hash $ outKeyPad `append` (hash $ inKeyPad `append` msg) where
    outKeyPad = pack $ zipWith (xor) key (repeat (0x5c :: Word8))
    inKeyPad = pack $ zipWith (xor) key (repeat (0x36 :: Word8))

realHmac = hmac key msg

eq' x y = foldl cmp (return True) $ zip x y where
cmp b (a, a') = do 
    ans <- b
    case ans of True  -> threadDelay 500000 >> return (a == a')
                False -> return False

checkHmac key msg hmc = do 
    let realHmc = hmac key msg
    isAut <- eq' realHmc hmc
    isAut
