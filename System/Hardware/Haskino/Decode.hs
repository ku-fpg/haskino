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
import           Data.Int
import           Data.Word
import           System.Hardware.Haskino.Data
import           System.Hardware.Haskino.Expr
import           System.Hardware.Haskino.Utils

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
decodeCmd Empty        = "Empty Command"
decodeCmd (x :< Empty) = show (firmwareValCmd x)
decodeCmd (x :< xs)    = show cmd ++ decoded ++ "\n"
  where
    cmd = firmwareValCmd x
    (dec, bs) = decodeCmdArgs cmd xs
    decoded = case bs of
                Empty -> dec
                _     -> dec ++ show (encode bs)

decodeCmdArgs :: FirmwareCmd -> B.ByteString -> (String, B.ByteString)
decodeCmdArgs BC_CMD_SYSTEM_RESET xs = ("", xs)
decodeCmdArgs BC_CMD_SET_PIN_MODE xs = decodeExprCmd 1 xs
decodeCmdArgs BC_CMD_DELAY_MILLIS xs = decodeExprProc 1 xs
decodeCmdArgs BC_CMD_DELAY_MICROS xs = decodeExprCmd 1 xs
decodeCmdArgs BC_CMD_LOOP xs = ("\n" ++ (decodeCodeBlock xs "Loop"), B.empty)
decodeCmdArgs BC_CMD_WHILE xs = (dec ++ dec' ++ "\n" ++ dec'', B.empty)
  where
    (dec, xs') = decodeExprCmd 2 xs
    (dec', xs'') = decodeExprCmd 1 (B.tail xs')
    dec'' = decodeCodeBlock xs'' "While"
decodeCmdArgs BC_CMD_IF_THEN_ELSE xs = (dec ++ "\n" ++ dec' ++ dec'', B.empty)
  where
    thenSize = bytesToWord16 (B.head xs, B.head (B.tail xs))
    (dec, xs') = decodeExprCmd 1 (B.drop 2 xs)
    dec' = decodeCodeBlock (B.take (fromIntegral thenSize) xs') "Then"
    dec'' = decodeCodeBlock (B.drop (fromIntegral thenSize) xs') "Else"
decodeCmdArgs BC_CMD_FORIN xs = (dec ++ "\n" ++ dec', B.empty)
  where
    (dec, xs') = decodeExprCmd 2 xs
    dec' = decodeCodeBlock xs' "ForIn"
decodeCmdArgs BS_CMD_REQUEST_VERSION xs = decodeExprProc 0 xs
decodeCmdArgs BS_CMD_REQUEST_TYPE xs = decodeExprProc 0 xs
decodeCmdArgs BS_CMD_REQUEST_MICROS xs = decodeExprProc 0 xs
decodeCmdArgs BS_CMD_REQUEST_MILLIS xs = decodeExprProc 0 xs
decodeCmdArgs DIG_CMD_READ_PIN xs = decodeExprProc 1 xs
decodeCmdArgs DIG_CMD_WRITE_PIN xs = decodeExprCmd 2 xs
decodeCmdArgs DIG_CMD_READ_PORT xs = decodeExprProc 2 xs
decodeCmdArgs DIG_CMD_WRITE_PORT xs = decodeExprCmd 3 xs
decodeCmdArgs ALG_CMD_READ_PIN xs = decodeExprProc 1 xs
decodeCmdArgs ALG_CMD_WRITE_PIN xs = decodeExprCmd 2 xs
decodeCmdArgs ALG_CMD_TONE_PIN xs = decodeExprCmd 3 xs
decodeCmdArgs ALG_CMD_NOTONE_PIN xs = decodeExprCmd 1 xs
decodeCmdArgs I2C_CMD_CONFIG xs = decodeExprCmd 0 xs
decodeCmdArgs I2C_CMD_READ xs = decodeExprProc 2 xs
decodeCmdArgs I2C_CMD_WRITE xs = decodeExprCmd 2 xs
decodeCmdArgs STEP_CMD_2PIN xs = decodeExprCmd 3 xs
decodeCmdArgs STEP_CMD_4PIN xs = decodeExprCmd 5 xs
decodeCmdArgs STEP_CMD_SET_SPEED xs = decodeExprCmd 2 xs
decodeCmdArgs STEP_CMD_STEP xs = decodeExprCmd 2 xs
decodeCmdArgs SRVO_CMD_ATTACH xs = decodeExprCmd 3 xs
decodeCmdArgs SRVO_CMD_DETACH xs = decodeExprCmd 1 xs
decodeCmdArgs SRVO_CMD_WRITE xs = decodeExprCmd 2 xs
decodeCmdArgs SRVO_CMD_WRITE_MICROS xs = decodeExprCmd 2 xs
decodeCmdArgs SRVO_CMD_READ xs = decodeExprProc 1 xs
decodeCmdArgs SRVO_CMD_READ_MICROS xs = decodeExprProc 1 xs
decodeCmdArgs SCHED_CMD_CREATE_TASK xs = decodeExprCmd 1 xs
decodeCmdArgs SCHED_CMD_DELETE_TASK xs = decodeExprCmd 1 xs
decodeCmdArgs SCHED_CMD_ADD_TO_TASK xs = ("", xs) -- TBD
decodeCmdArgs SCHED_CMD_SCHED_TASK xs = decodeExprCmd 2 xs
decodeCmdArgs SCHED_CMD_QUERY_ALL xs = decodeExprProc 0 xs
decodeCmdArgs SCHED_CMD_QUERY xs = decodeExprProc 1 xs
decodeCmdArgs SCHED_CMD_RESET xs =  decodeExprCmd 0 xs
decodeCmdArgs SCHED_CMD_BOOT_TASK xs = decodeExprCmd 1 xs
decodeCmdArgs REF_CMD_NEW xs = decodeRefNew 1 xs
decodeCmdArgs REF_CMD_READ xs =  decodeRefProc 1 xs
decodeCmdArgs REF_CMD_WRITE xs = decodeRefCmd 2 xs
decodeCmdArgs UNKNOWN_COMMAND xs = ("Unknown Command", xs)

decodeExprCmd :: Int -> B.ByteString -> (String, B.ByteString)
decodeExprCmd cnt bs = decodeExprCmd' cnt "" bs
  where
    decodeExprCmd' :: Int -> String -> B.ByteString -> (String, B.ByteString)
    decodeExprCmd' cnt dec bs = 
        if (cnt == 0)
        then ("", bs)
        else (dec' ++ dec'', bs'')
      where
        (dec', bs') = decodeExpr bs
        (dec'', bs'') = decodeExprCmd' (cnt-1) dec' bs'

decodeExprProc :: Int -> B.ByteString -> (String, B.ByteString)
decodeExprProc cnt bs = (" (Bind " ++ show b ++ ") <-" ++ c, bs')
  where
    b = B.head bs
    (c, bs') = decodeExprCmd cnt (B.tail bs)

decodeRefCmd :: Int -> B.ByteString -> (String, B.ByteString)
decodeRefCmd cnt bs =
  case bs of
    Empty     -> decodeErr bs
    (x :< xs) -> ("-" ++ (show ((toEnum (fromIntegral x))::RefType)) ++ " " ++ dec, bs')
  where
    (dec, bs') = decodeExprCmd cnt (B.tail bs)

decodeRefProc :: Int -> B.ByteString -> (String, B.ByteString)
decodeRefProc cnt bs = 
  case bs of
    Empty         -> decodeErr bs
    (x :< Empty) -> decodeErr bs
    (x :< y :< xs) -> ("-" ++ (show ((toEnum (fromIntegral x))::RefType)) ++ " (Bind " ++ show y ++ ") <-"++ dec, bs')
  where
    (dec, bs') = decodeExprCmd cnt (B.tail $ B.tail bs)

decodeRefNew :: Int -> B.ByteString -> (String, B.ByteString)
decodeRefNew cnt bs = 
  case bs of
    Empty               -> decodeErr bs
    (x :< Empty)        -> decodeErr bs
    (x :< y :< Empty)   -> decodeErr bs
    (x :< _ :< z :< xs) -> ("-" ++ (show ((toEnum (fromIntegral x))::RefType)) ++ " (Ref Index " ++ show z ++ ") "++ dec, bs')
  where
    (dec, bs') = decodeExprCmd cnt (B.drop 3 bs)

decodeErr :: B.ByteString -> (String, B.ByteString)
decodeErr bs = ("Decode Error, remaining=" ++ show (encode bs), B.empty)

decodeCodeBlock :: B.ByteString -> String -> String
decodeCodeBlock bs desc = 
  "*** Start of " ++ desc ++ " body:\n" ++ 
  (decodeCmds $ decodeCodeBlock' bs []) ++ 
  "*** End of " ++ desc ++ " body\n"
    where
      decodeCodeBlock' :: B.ByteString -> [B.ByteString] -> [B.ByteString]
      decodeCodeBlock' bs cmds = 
        case bs of
          Empty                -> cmds
          (x :< xs) | x < 0xFF -> decodeCodeBlock' (B.drop (fromIntegral x) xs) (cmds ++ [B.take (fromIntegral x) xs])
          (x :< y :< xs)       -> decodeCodeBlock' (B.drop (fromIntegral x) xs) (cmds ++ [B.take (fromIntegral x) xs])

decodeExpr :: B.ByteString -> (String, B.ByteString)
decodeExpr Empty = decodeErr B.empty
decodeExpr bs    = (" (" ++ opStr ++ ")", bs') 
  where
    (etype, op) = byteToTypeOp $ B.head bs
    (opStr, bs') = decodeTypeOp etype op (B.tail bs)

decodeTypeOp :: Either ExprType ExprExtType -> Int -> B.ByteString -> (String, B.ByteString)
decodeTypeOp etype op bs = 
  case etype of
    (Left lt)  -> (show lt ++ "-" ++ show eop ++ deop, bs')
    (Right rt) -> case rt of
                   EXPR_LIST8 -> (show rt ++ "-" ++ show elop ++ deop, bs'')
                   EXPR_FLOAT -> (show rt ++ "-" ++ show efop ++ deop, bs''')
  where
    eop = toEnum op::ExprOp
    elop = toEnum op::ExprListOp
    efop = toEnum op::ExprFloatOp
    (deop, bs') = case etype of
                    (Left lt) -> decodeOp lt eop bs
    (delop, bs'') = decodeListOp elop bs
    (defop, bs''') = decodeFloatOp efop bs

decodeOp :: ExprType -> ExprOp -> B.ByteString -> (String, B.ByteString)
decodeOp etype eop bs = 
  case eop of
    EXPR_LIT   -> decodeLit etype bs 
    EXPR_REF   -> decodeRefBind bs
    EXPR_BIND  -> decodeRefBind bs
    EXPR_EQ    -> decodeExprOps 2 "" bs
    EXPR_LESS  -> decodeExprOps 2 "" bs
    EXPR_IF    -> decodeExprOps 3 "" bs
    EXPR_FINT  -> decodeExprOps 1 "" bs
    EXPR_NEG   -> decodeExprOps 1 "" bs
    EXPR_SIGN  -> decodeExprOps 1 "" bs
    EXPR_ADD   -> decodeExprOps 2 "" bs
    EXPR_SUB   -> decodeExprOps 2 "" bs
    EXPR_MULT  -> decodeExprOps 2 "" bs
    EXPR_DIV   -> decodeExprOps 2 "" bs
    EXPR_NOT   -> decodeExprOps 1 "" bs
    EXPR_AND   -> decodeExprOps 2 "" bs
    EXPR_OR    -> decodeExprOps 2 "" bs
    EXPR_TINT  -> decodeExprOps 1 "" bs
    EXPR_XOR   -> decodeExprOps 2 "" bs
    EXPR_REM   -> decodeExprOps 2 "" bs 
    EXPR_COMP  -> decodeExprOps 2 "" bs 
    EXPR_SHFL  -> decodeExprOps 2 "" bs 
    EXPR_SHFR  -> decodeExprOps 2 "" bs 
    EXPR_TSTB  -> decodeExprOps 2 "" bs 
    EXPR_SETB  -> decodeExprOps 2 "" bs
    EXPR_CLRB  -> decodeExprOps 2 "" bs
    EXPR_QUOT  -> decodeExprOps 2 "" bs
    EXPR_MOD   -> decodeExprOps 2 "" bs
    EXPR_SHOW  -> decodeExprOps 1 "" bs

decodeRefBind :: B.ByteString -> (String, B.ByteString)
decodeRefBind bs = 
  case bs of
    Empty    -> decodeErr bs
    (x :< xs) -> (show x, xs)

decodeLit :: ExprType -> B.ByteString -> (String, B.ByteString)
decodeLit etype bs =
  case etype of
    EXPR_BOOL   -> case bs of
                     Empty     -> decodeErr bs
                     (x :< xs) -> (if x==0 then " False" else " True", xs)
    EXPR_WORD8  -> case bs of
                     Empty     -> decodeErr bs
                     (x :< xs) -> (" " ++ show x, xs)
    EXPR_WORD16 -> case bs of
                     Empty          -> decodeErr bs
                     (x :< Empty)   -> decodeErr bs
                     (x :< y :< xs) -> (" " ++ (show $ bytesToWord16 (x,y)), xs)
    EXPR_WORD32 -> case bs of
                     Empty               -> decodeErr bs
                     (x :< xs) 
                      | B.length xs < 3  -> decodeErr bs
                     (x :< y :< z :< a :< xs) -> (" " ++ (show $ bytesToWord32 (x,y,z,a)), xs)
    EXPR_INT8   -> case bs of
                     Empty     -> decodeErr bs
                     (x :< xs) -> (" " ++ show ((fromIntegral x)::Int8), xs)
    EXPR_INT16  -> case bs of
                     Empty          -> decodeErr bs
                     (x :< Empty)   -> decodeErr bs
                     (x :< y :< xs) -> (" " ++ show ((fromIntegral $ bytesToWord16 (x,y))::Int16), xs)
    EXPR_INT32 -> case bs of
                     Empty               -> decodeErr bs
                     (x :< xs) 
                      | B.length xs < 3  -> decodeErr bs
                     (x :< y :< z :< a :< xs) -> (" " ++ show ((fromIntegral $ bytesToWord32 (x,y,z,a))::Int32), xs)

decodeExprOps :: Int -> String -> B.ByteString -> (String, B.ByteString)
decodeExprOps cnt dec bs = 
    if (cnt == 0)
    then ("", bs)
    else (dec' ++ dec'', bs'')
  where
    (dec', bs') = decodeExpr bs 
    (dec'', bs'') = decodeExprOps (cnt-1) dec' bs'

decodeListOp :: ExprListOp -> B.ByteString -> (String, B.ByteString)
decodeListOp elop bs =
  case elop of
    EXPRL_LIT  -> case bs of
                    Empty     -> decodeErr bs
                    (x :< xs) -> decodeListLit (fromIntegral x) xs
    EXPRL_REF  -> decodeRefBind bs
    EXPRL_BIND -> decodeRefBind bs
    EXPRL_EQ   -> decodeExprOps 2 "" bs
    EXPRL_LESS -> decodeExprOps 2 "" bs
    EXPRL_IF   -> decodeExprOps 3 "" bs
    EXPRL_ELEM -> decodeExprOps 2 "" bs
    EXPRL_LEN  -> decodeExprOps 1 "" bs
    EXPRL_CONS -> decodeExprOps 2 "" bs
    EXPRL_APND -> decodeExprOps 2 "" bs
    EXPRL_PACK -> case bs of
                    Empty     -> decodeErr bs
                    (x :< xs) -> decodeListPack (fromIntegral x) xs
  where
    decodeListLit :: Int -> B.ByteString -> (String, B.ByteString)
    decodeListLit cnt bs = (show $ B.unpack $ B.take cnt bs, B.drop cnt bs)
 
    decodeListPack :: Int -> B.ByteString -> (String, B.ByteString)
    decodeListPack cnt bs = ("[" ++ dec ++ "]", bs')
      where
        (dec, bs') = decodeExprOps cnt "" bs

decodeFloatOp :: ExprFloatOp -> B.ByteString -> (String, B.ByteString)
decodeFloatOp efop bs = 
  case efop of
    EXPRF_LIT   -> case bs of  
                     Empty               -> decodeErr bs
                     (x :< xs) 
                      | B.length xs < 3  -> decodeErr bs
                     (x :< y :< z :< a :< xs) -> (" " ++ show (bytesToFloat (x,y,z,a)), xs)
    EXPRF_REF   -> decodeRefBind bs
    EXPRF_BIND  -> decodeRefBind bs
    EXPRF_EQ    -> decodeExprOps 2 "" bs
    EXPRF_LESS  -> decodeExprOps 2 "" bs
    EXPRF_IF    -> decodeExprOps 3 "" bs
    EXPRF_FINT  -> decodeExprOps 1 "" bs
    EXPRF_NEG   -> decodeExprOps 1 "" bs
    EXPRF_SIGN  -> decodeExprOps 1 "" bs
    EXPRF_ADD   -> decodeExprOps 2 "" bs
    EXPRF_SUB   -> decodeExprOps 2 "" bs
    EXPRF_MULT  -> decodeExprOps 2 "" bs
    EXPRF_DIV   -> decodeExprOps 2 "" bs
    EXPRF_SHOW  -> decodeExprOps 1 "" bs
    EXPRF_MATH  -> case bs of
                     Empty     -> decodeErr bs
                     (x :< xs) -> decodeMathOp (toEnum ((fromIntegral x)::Int)) bs

decodeMathOp :: ExprFloatMathOp -> B.ByteString -> (String, B.ByteString)
decodeMathOp efmop bs =
  case efmop of
    EXPRF_TRUNC  -> decodeExprOps 1 "" bs
    EXPRF_FRAC   -> decodeExprOps 1 "" bs
    EXPRF_ROUND  -> decodeExprOps 1 "" bs
    EXPRF_CEIL   -> decodeExprOps 1 "" bs
    EXPRF_FLOOR  -> decodeExprOps 1 "" bs
    EXPRF_PI     -> decodeExprOps 0 "" bs
    EXPRF_EXP    -> decodeExprOps 1 "" bs
    EXPRF_LOG    -> decodeExprOps 1 "" bs
    EXPRF_SQRT   -> decodeExprOps 1 "" bs
    EXPRF_SIN    -> decodeExprOps 1 "" bs
    EXPRF_COS    -> decodeExprOps 1 "" bs
    EXPRF_TAN    -> decodeExprOps 1 "" bs
    EXPRF_ASIN   -> decodeExprOps 1 "" bs
    EXPRF_ACOS   -> decodeExprOps 1 "" bs
    EXPRF_ATAN   -> decodeExprOps 1 "" bs
    EXPRF_ATAN2  -> decodeExprOps 2 "" bs
    EXPRF_SINH   -> decodeExprOps 1 "" bs
    EXPRF_COSH   -> decodeExprOps 1 "" bs
    EXPRF_TANH   -> decodeExprOps 1 "" bs
    EXPRF_POWER  -> decodeExprOps 2 "" bs
    EXPRF_ISNAN  -> decodeExprOps 1 "" bs
    EXPRF_ISINF  -> decodeExprOps 1 "" bs

byteToTypeOp :: Word8 -> (Either ExprType ExprExtType, Int)
byteToTypeOp b = if (byteTypeNum b) < 7
                 then (Left $ toEnum $ fromIntegral $ byteTypeNum b, 
                       fromIntegral $ byteOpNum b)
                 else (Right $ exprExtValType $ byteExtTypeNum b, 
                       fromIntegral $ byteExtOpNum b)
  where 
    byteTypeNum :: Word8 -> Word8
    byteTypeNum b = (b .&. 0xE0) `shiftR` 5

    byteExtTypeNum :: Word8 -> Word8
    byteExtTypeNum b = (b .&. 0xF0) `shiftR` 4

    byteOpNum :: Word8 -> Word8
    byteOpNum b = b .&. 0x1F

    byteExtOpNum :: Word8 -> Word8
    byteExtOpNum b = b .&. 0x1F
