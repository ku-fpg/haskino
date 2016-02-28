-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.Utils
--                Based on System.Hardware.Arduino.Utils
-- Copyright   :  (c) University of Kansas
--                System.Hardware.Arduino (c) Levent Erkok
-- License     :  BSD3
-- Stability   :  experimental
--
-- Internal utilities
-------------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

module System.Hardware.Haskino.Decode where

import           Data.Bits
import qualified Data.ByteString  as B 
import           Data.ByteString.Base16 (encode)
import           Data.Foldable (foldMap)
import           Data.Word
import           System.Hardware.Haskino.Data
import           System.Hardware.Haskino.Expr

infixr 5 :<

pattern b :< bs <- (B.uncons -> Just (b, bs))
pattern Empty   <- (B.uncons -> Nothing)

deframe :: B.ByteString -> [B.ByteString]
deframe bs = map unescape (deframe' bs [])
  where
    notFrameChar :: Word8 -> Bool
    notFrameChar b = b /= 0x7e

    deframe' :: B.ByteString -> [B.ByteString] -> [B.ByteString]
    deframe' Empty        xs = xs
    deframe' bs           xs = deframe' (tailFrame bs) (xs ++ [headFrame bs])

    headFrame :: B.ByteString -> B.ByteString  
    headFrame bs = B.init $ B.takeWhile notFrameChar bs

    tailFrame :: B.ByteString -> B.ByteString  
    tailFrame bs = B.tail $ B.dropWhile notFrameChar bs

    unescape :: B.ByteString -> B.ByteString
    unescape Empty        = B.empty
    unescape (x :< Empty) = B.singleton x
    unescape (x :< y :< xs)
      | x == 0x7d && y == 0x5d =  B.cons 0x7d (unescape xs)
      | x == 0x7d && y == 0x5e =  B.cons 0x7e (unescape xs)
      | otherwise              =  B.cons x (unescape (B.cons y xs))

decodeFrame :: B.ByteString -> String
decodeFrame bs = decodeCmds $ deframe bs

decodeCmds :: [B.ByteString] -> String
decodeCmds cs = concat $ map decodeCmd cs

decodeCmd :: B.ByteString -> String
decodeCmd Empty        = "EmptyCommand"
decodeCmd (x :< Empty) = show (firmwareValCmd x)
decodeCmd (x :< xs)    = show (firmwareValCmd x) ++ "[" ++ show (encode xs) ++ "]\n"

byteToTypeOp :: Word8 -> (Either ExprType ExprExtType, Int)
byteToTypeOp b = if (byteTypeNum b) < 7
                 then (Left $ toEnum $ fromIntegral $ byteTypeNum b, 
                       fromIntegral $ byteOpNum b)
                 else (Right $ exprExtValType $ byteExtTypeNum b, 
                       fromIntegral $ byteExtOpNum b)
  where 
    byteTypeNum :: Word8 -> Word8
    byteTypeNum b = (b .&. 0xE0) `shiftL` 5

    byteExtTypeNum :: Word8 -> Word8
    byteExtTypeNum b = (b .&. 0xF0) `shiftL` 4

    byteOpNum :: Word8 -> Word8
    byteOpNum b = b .&. 0x1F

    byteExtOpNum :: Word8 -> Word8
    byteExtOpNum b = b .&. 0x1F


