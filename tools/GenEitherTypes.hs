-------------------------------------------------------------------------------
-- |
-- Module      :  GenEitherTypes
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- The /hello world/ of the arduino world, blinking the led.
-------------------------------------------------------------------------------

module Main where

typeNameList       = ["Unit", "Bool", "W8", "W16", "W32",
                      "I8", "I16", "I32", "L8", "Float"]
typeList           = ["()", "Bool", "Word8", "Word16", "Word32",
                      "Int8", "Int16", "Int32", "[Word8]", "Float"]
bindList           = ["()", "B", "W8", "W16", "32",
                      "I8", "I16", "I32", "List8", "Float"]
compTypeList       = ["Unit", "Bool", "Word8", "Word16", "Word32",
                      "Int8", "Int16", "Int32", "List8", "Float"]
typeNameTypeList   = zip typeNameList typeList
typeNameTypeCombos = [ (x,y) | x<-typeNameTypeList, y<-typeNameTypeList ]
typeNameBindList   = zip typeNameList bindList
typeNameBindCombos = [ (x,y) | x<-typeNameBindList, y<-typeNameBindList ]
typeNameCompTypeList   = zip typeNameList compTypeList
typeNameCompTypeCombos = [ (x,y) | x<-typeNameCompTypeList, y<-typeNameCompTypeList ]
typeNameBindCompTypeList   = zip3 typeNameList bindList compTypeList
typeNameBindCompTypeCombos = [ (x,y) | x<-typeNameBindCompTypeList, y<-typeNameBindCompTypeList ]

genEitherPrim :: ((String, String) , (String, String)) -> String
genEitherPrim ((tyn1,ty1), (tyn2, ty2)) = 
    "     IfThenElse" ++ tyn1 ++ tyn2 ++ spaces ++ ":: Expr Bool" ++ argStrs ++ returnStr ++ "\n"
  where
    strLens = (length tyn1) + (length tyn2)
    spaces = replicate (11 - strLens) ' '
    argStr = " -> Arduino (ExprEither " ++ ty1 ++ " " ++ ty2 ++ ")"
    argStrs = concat $ replicate 2 argStr
    returnStr = " -> ArduinoPrimitive (ExprEither " ++ ty1 ++ " " ++ ty2 ++ ")"

genIteratePrim :: ((String, String) , (String, String)) -> String
genIteratePrim ((tyn1,ty1), (tyn2, ty2)) = 
    "     Iterate" ++ tyn1 ++ tyn2 ++ "E" ++ spaces ++ ":: Expr " ++ ty1 ++ 
    " -> (Expr " ++ ty1 ++ " -> Arduino (ExprEither " ++ ty1 ++ " " ++
    ty2 ++ " )) -> ArduinoPrimitive (Expr " ++ ty2 ++ " )" ++ "\n"
  where
    strLens = (length tyn1) + (length tyn2)
    spaces = replicate (13 - strLens) ' '

genIterateInst :: ((String, String) , (String, String)) -> String
genIterateInst ((tyn1,ty1), (tyn2, ty2)) = 
    "instance ArduinoIterate " ++ ty1 ++ " " ++ ty2 ++ " where\n" ++
    "    iterateE iv bf = Arduino $ primitive $ Iterate" ++ tyn1 ++ tyn2 ++ "E iv bf\n" ++
    "    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElse"++ tyn1 ++ tyn2 ++ " be te ee\n\n"

genShowIf :: ((String, String) , (String, String)) -> String
genShowIf ((tyn1, _), (tyn2, _)) = 
    "      showProcedure (IfThenElse" ++ tyn1 ++ tyn2 ++ " e cb1 cb2) =\n" ++
    "          showIfThenElseEitherProcedure e cb1 cb2\n"

genShowIter :: ((String, String) , (String, String)) -> String
genShowIter ((tyn1,ty1), (tyn2, ty2)) = 
    "      showProcedure (Iterate" ++ tyn1 ++ tyn2 ++ "E iv bf) = do\n" ++
    "          i <- nextBind\n" ++
    "          let bi = RemBind" ++ ty1 ++ " i\n" ++
    "          j <- nextBind\n" ++
    "          let bj = RemBind" ++ ty2 ++ " i\n" ++
    "          showIterateProcedure i bi j bj iv bf\n" ++
    "          return bj\n"

genCompIf :: ((String, String) , (String, String)) -> String
genCompIf ((tyn1, ty1), (tyn2, ty2)) = 
    "compileProcedure (IfThenElse" ++ tyn1 ++ tyn2 ++ " e cb1 cb2) =\n" ++
    "    compileIfThenElseEitherProcedure " ++ ty1 ++ "Type " ++ ty2 ++ "Type e cb1 cb2\n"

genCompIter :: ((String, String, String) , (String, String, String)) -> String
genCompIter ((tyn1, ty1, cty1), (tyn2, ty2, cty2)) = 
    "compileProcedure (Iterate" ++ tyn1 ++ tyn2 ++ "E iv bf) = do\n" ++
    "    i <- nextBind\n" ++
    "    let bi = RemBind" ++ ty1 ++ " i\n" ++
    "    j <- nextBind\n" ++
    "    let bj = RemBind" ++ ty2 ++ " i\n" ++
    "    compileIterateProcedure " ++ cty1 ++ "Type " ++ cty2 ++ "Type i bi j bj iv bf\n" ++
    "    return bj\n"

main :: IO ()
main = do
  let prims1 = map genEitherPrim typeNameTypeCombos
  let prims2 = map genIteratePrim typeNameTypeCombos
  let class1 = map genIterateInst typeNameTypeCombos
  let showIfs = map genShowIf typeNameTypeCombos
  let showIters = map genShowIter typeNameBindCombos
  let compIfs = map genCompIf typeNameCompTypeCombos
  let compIters = map genCompIter typeNameBindCompTypeCombos
  putStrLn $ concat prims1
  putStrLn ""
  putStrLn $ concat prims2
  putStrLn ""
  putStrLn $ concat class1
  putStrLn ""
  putStrLn $ concat showIfs
  putStrLn ""
  putStrLn $ concat showIters
  putStrLn ""
  putStrLn $ concat compIfs
  putStrLn ""
  putStrLn $ concat compIters

