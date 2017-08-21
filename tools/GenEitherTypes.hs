-------------------------------------------------------------------------------
-- |
-- Module      :  GenEitherTypes
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Generator for Haskino Ether types
-------------------------------------------------------------------------------

module Main where

typeNameList       = ["Unit", "Bool", "W8", "W16", "W32",
                      "I8", "I16", "I32", "I", "L8", "Float"]
typeList           = ["()", "Bool", "Word8", "Word16", "Word32",
                      "Int8", "Int16", "Int32", "Int", "[Word8]", "Float"]
bindList           = ["Unit", "B", "W8", "W16", "W32",
                      "I8", "I16", "I32", "I", "List8", "Float"]
compTypeList       = ["Unit", "Bool", "Word8", "Word16", "Word32",
                      "Int8", "Int16", "Int32", "Int", "List8", "Float"]
packTypeList       = ["UNIT", "BOOL", "WORD8", "WORD16", "WORD32",
                      "INT8", "INT16", "INT32", "INT", "LIST8", "FLOAT"]
typeNameTypeList   = zip typeNameList typeList
typeNameTypeCombos = [ (x,y) | x<-typeNameTypeList, y<-typeNameTypeList ]
typeNameCombos = [ (x,y)  | x<-typeNameList, y<-typeNameList]
typeNameBindList   = zip typeNameList bindList
typeNameBindCombos = [ (x,y) | x<-typeNameBindList, y<-typeNameBindList ]
typeNameCompTypeList   = zip typeNameList compTypeList
typeNameCompTypeCombos = [ (x,y) | x<-typeNameCompTypeList, y<-typeNameCompTypeList ]
typeNameBindCompTypeList   = zip3 typeNameList bindList compTypeList
typeNameBindCompTypeCombos = [ (x,y) | x<-typeNameBindCompTypeList, y<-typeNameBindCompTypeList ]
typeNameBindPackTypeList   = zip3 typeNameList bindList packTypeList
typeNameBindPackTypeCombos = [ (x,y) | x<-typeNameBindPackTypeList, y<-typeNameBindPackTypeList ]
typeNamePackTypeList = zip typeNameList packTypeList
typeNamePackTypeCombos = [ (x,y) | x<-typeNamePackTypeList, y<-typeNamePackTypeList ]

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
    ty2 ++ ")) -> ArduinoPrimitive (Expr " ++ ty2 ++ ")" ++ "\n"
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
    "          let bj = RemBind" ++ ty2 ++ " j\n" ++
    "          _ <- showIterateProcedure i bi j bj iv bf\n" ++
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
    "    let bj = RemBind" ++ ty2 ++ " j\n" ++
    "    compileIterateProcedure " ++ cty1 ++ "Type " ++ cty2 ++ "Type i bi j bj iv bf\n" ++
    "    return bj\n"

genPackIf :: ((String, String) , (String, String)) -> String
genPackIf ((tyn1,ty1), (tyn2,_)) =
    "      packProcedure (IfThenElse" ++ tyn1 ++ tyn2 ++ " e cb1 cb2) = do\n" ++
    "          i <- packIfEitherProcedure (IfThenElse" ++ tyn1 ++ tyn2 ++ " e cb1 cb2)\n" ++
    "          return $ ExprLeft $ RemBind" ++ ty1 ++ " i\n"

genPackIter :: ((String, String) , (String, String)) -> String
genPackIter ((tyn1,_), (tyn2, ty2)) =
    "      packProcedure (Iterate" ++ tyn1 ++ tyn2 ++ "E iv bf) = do\n" ++
    "          i <- packIterateProcedure (Iterate" ++ tyn1 ++ tyn2 ++ "E iv bf)\n" ++
    "          return $ RemBind" ++ ty2 ++ " i\n"

genPackageIf :: ((String, String) , (String, String)) -> String
genPackageIf ((tyn1,ty1), (tyn2,ty2)) =
    "    packageProcedure' (IfThenElse" ++ tyn1 ++ tyn2 ++ " e cb1 cb2) ib = packageIfThenElseEitherProcedure EXPR_" ++ ty1 ++ " EXPR_" ++ ty2 ++ " ib e cb1 cb2\n"

genPackageIter :: ((String, String, String) , (String, String, String)) -> String
genPackageIter ((tyn1, ty1, cty1), (tyn2, _, cty2)) =
    "    packageProcedure' (Iterate" ++ tyn1 ++ tyn2 ++ "E iv bf) ib = packageIterateProcedure EXPR_" ++ cty1 ++ " EXPR_" ++ cty2 ++ " ib (RemBind" ++ ty1  ++ " ib) iv bf\n"

genParseIf :: (String, String) -> String
genParseIf (tyn1, tyn2) =
      "parseQueryResult (IfThenElse" ++ tyn1 ++ tyn2 ++ " _ _ _) (IfThenElse" ++ tyn2 ++ "Reply r) = Just $ ExprRight $ lit r\n" ++
      "parseQueryResult (IfThenElse" ++ tyn1 ++ tyn2 ++ " _ _ _) (IfThenElse" ++ tyn1 ++ "LeftReply r) = Just $ ExprLeft $ lit r\n"

genParseIter :: (String, String) -> String
genParseIter (tyn1,tyn2) =
      "parseQueryResult (Iterate" ++ tyn1 ++ tyn2 ++ "E _ _) (Iterate" ++ tyn2 ++
      if tyn2 == "Unit" then "Reply) = Just $ LitUnit\n" else "Reply r) = Just $ lit r\n"

genCommProcIf :: (String, String) -> String
genCommProcIf (tyn1, tyn2) =
      "              , \"ifThenElse" ++ tyn1 ++ tyn2 ++ "\"\n"

genCommProcIter :: (String, String) -> String
genCommProcIter (tyn1, tyn2) =
      "              , \"iterate" ++ tyn1 ++ tyn2 ++ "E\"\n"

main :: IO ()
main = do
  let prims1 = map genEitherPrim typeNameTypeCombos
  let prims2 = map genIteratePrim typeNameTypeCombos
  let class1 = map genIterateInst typeNameTypeCombos
  let showIfs = map genShowIf typeNameTypeCombos
  let showIters = map genShowIter typeNameBindCombos
  let compIfs = map genCompIf typeNameCompTypeCombos
  let compIters = map genCompIter typeNameBindCompTypeCombos
  let packIfs = map genPackIf typeNameBindCombos
  let packIters = map genPackIter typeNameBindCombos
  let packageIfs = map genPackageIf typeNamePackTypeCombos
  let packageIters = map genPackageIter typeNameBindPackTypeCombos
  let parseIfs = map genParseIf typeNameCombos
  let parseIters = map genParseIter typeNameCombos
  let commProcIfs = map genCommProcIf typeNameCombos
  let commProcIters = map genCommProcIter typeNameCombos
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
  putStrLn ""
  putStrLn $ concat packIfs
  putStrLn ""
  putStrLn $ concat packIters
  putStrLn ""
  putStrLn $ concat packageIfs
  putStrLn ""
  putStrLn $ concat packageIters
  putStrLn ""
  putStrLn $ concat parseIfs
  putStrLn ""
  putStrLn $ concat parseIters
  putStrLn ""
  putStrLn $ concat commProcIfs
  putStrLn ""
  putStrLn $ concat commProcIters
