module Generator where
  import Control.Monad.State
  import Data.Char
  import Data.List
  import Data.Map (Map)
  import qualified Data.Map as Map
  import Data.Set (fromList)
  import Lexer
  import Parser
  import Syntax

  data GeneratorState = GeneratorState {list :: [(Maybe IRLabel, IRInstruction)], counter :: Integer, table :: Map String (IRLabel, IRType)}

  type GeneratorStateMonad = State GeneratorState

  getIdentifier (CDeclarator _ (CDirectDeclaratorIdentifier (CIdentifier (CIdentifierToken a)))) = a
  getIdentifier (CInitDeclarator (CDeclarator _ (CDirectDeclaratorIdentifier (CIdentifier (CIdentifierToken a)))) _) = a

  getPointer (CDeclarator a _) = a
  getPointer (CInitDeclarator a _) = getPointer a

  getConstant (CInitDeclarator _ a) = a
  getConstant (CDeclarator _ _) = CConstant (CConstantToken (CIntegerConstant 0))

  getType (IRMul a _ _) = a
  getType (IRLoad a _ _) = a
  getType (IRStore a _ _ _) = a

  isSameType a b
    | isIRInteger a && isIRInteger b || a == b = (Just b)
    | otherwise = Nothing

  isIRInteger a = a == IRShortInteger IRSigned || a == IRShortInteger IRUnsigned || a == IRInteger IRSigned || a == IRInteger IRUnsigned || a == IRLongInteger IRSigned || a == IRLongInteger IRUnsigned

  isIRFloating a = a == IRFloat || a == IRDouble || a == IRLongDouble

  generateIRType a (Just (CPointer _)) = IRPointer (generateIRType a Nothing)

  generateIRType (CSpecifiers a) Nothing
    | fromList a == fromList [CTypeSpecifier (CKeywordToken "void")] = IRVoid
    | fromList a == fromList [CTypeSpecifier (CKeywordToken "short")] = IRShortInteger IRSigned
    | fromList a == fromList [CTypeSpecifier (CKeywordToken "signed"), CTypeSpecifier (CKeywordToken "short")] = IRShortInteger IRSigned
    | fromList a == fromList [CTypeSpecifier (CKeywordToken "short"), CTypeSpecifier (CKeywordToken "int")] = IRShortInteger IRSigned
    | fromList a == fromList [CTypeSpecifier (CKeywordToken "signed"), CTypeSpecifier (CKeywordToken "short"), CTypeSpecifier (CKeywordToken "int")] = IRShortInteger IRSigned
    | fromList a == fromList [CTypeSpecifier (CKeywordToken "unsigned"), CTypeSpecifier (CKeywordToken "short")] = IRShortInteger IRUnsigned
    | fromList a == fromList [CTypeSpecifier (CKeywordToken "unsigned"), CTypeSpecifier (CKeywordToken "short"), CTypeSpecifier (CKeywordToken "int")] = IRShortInteger IRUnsigned
    | fromList a == fromList [CTypeSpecifier (CKeywordToken "int")] = IRInteger IRSigned
    | fromList a == fromList [CTypeSpecifier (CKeywordToken "signed")] = IRInteger IRSigned
    | fromList a == fromList [CTypeSpecifier (CKeywordToken "signed"), CTypeSpecifier (CKeywordToken "int")] = IRInteger IRSigned
    | fromList a == fromList [CTypeSpecifier (CKeywordToken "unsigned")] = IRInteger IRUnsigned
    | fromList a == fromList [CTypeSpecifier (CKeywordToken "unsigned"), CTypeSpecifier (CKeywordToken "int")] = IRInteger IRUnsigned
    | fromList a == fromList [CTypeSpecifier (CKeywordToken "long")] = IRLongInteger IRSigned
    | fromList a == fromList [CTypeSpecifier (CKeywordToken "signed"), CTypeSpecifier (CKeywordToken "long")] = IRLongInteger IRSigned
    | fromList a == fromList [CTypeSpecifier (CKeywordToken "long"), CTypeSpecifier (CKeywordToken "int")] = IRLongInteger IRSigned
    | fromList a == fromList [CTypeSpecifier (CKeywordToken "signed"), CTypeSpecifier (CKeywordToken "long"), CTypeSpecifier (CKeywordToken "int")] = IRLongInteger IRSigned
    | fromList a == fromList [CTypeSpecifier (CKeywordToken "unsigned"), CTypeSpecifier (CKeywordToken "long")] = IRLongInteger IRUnsigned
    | fromList a == fromList [CTypeSpecifier (CKeywordToken "unsigned"), CTypeSpecifier (CKeywordToken "long"), CTypeSpecifier (CKeywordToken "int")] = IRLongInteger IRUnsigned
    | fromList a == fromList [CTypeSpecifier (CKeywordToken "char")] = IRInteger IRSigned
    | fromList a == fromList [CTypeSpecifier (CKeywordToken "signed"), CTypeSpecifier (CKeywordToken "char")] = IRInteger IRSigned
    | fromList a == fromList [CTypeSpecifier (CKeywordToken "unsigned"), CTypeSpecifier (CKeywordToken "char")] = IRInteger IRUnsigned
    | fromList a == fromList [CTypeSpecifier (CKeywordToken "float")] = IRFloat
    | fromList a == fromList [CTypeSpecifier (CKeywordToken "double")] = IRDouble
    | fromList a == fromList [CTypeSpecifier (CKeywordToken "long"), CTypeSpecifier (CKeywordToken "double")] = IRLongDouble

  generateIRConstant a b
    | b == IRFloat || b == IRDouble || b == IRLongDouble = IRFloatingConstant (value a)
    | otherwise = IRIntegerConstant ((floor . value) a)
    where
      value (CConstant (CConstantToken (CFloatingConstant a))) = a
      value (CConstant (CConstantToken (CIntegerConstant a))) = fromIntegral a
      value (CConstant (CConstantToken (CCharacterConstant a))) = (fromIntegral . ord) a
      value (CExpression [a]) = value a
      value _ = 0

  generateIRAlloca a = IRAlloca a Nothing Nothing

  generateIRStore a b c = IRStore a b c Nothing

  generateIRLoad a b = IRLoad a b Nothing

  generateIRBasicBlock (CCompound a b) c = evalState irBasicBlock (GeneratorState [] 1 Map.empty)
    where
      irBasicBlock :: GeneratorStateMonad [IRBasicBlock]
      irBasicBlock = do
        got <- get
        let dec = runState ((declarations . declarationList) a) got
        let stat = runState ((statements . statementList) b) (snd dec)
        return [IRBasicBlock "" (fst stat)]

      declarationList (Just (CDeclarationList a)) = a
      declarationList Nothing = []

      declarations :: [CDeclaration] -> GeneratorStateMonad [(Maybe IRLabel, IRInstruction)]
      declarations [] = do
        got <- get
        return (list got)

      declarations (a:as) = do
        declaration a
        declarations as

      declaration :: CDeclaration -> GeneratorStateMonad [(Maybe IRLabel, IRInstruction)]
      declaration (CDeclaration d (Just (CInitDeclaratorList e))) = do
        got <- get
        let irAllocaState = runState (irAlloca d e) (GeneratorState [] (counter got) (table got))
        let putCounter = (counter . snd) irAllocaState
        let putTable = (table . snd) irAllocaState
        let irStoreState = runState (irStore d e) (GeneratorState [] putCounter putTable)
        put (GeneratorState ((list got) ++ (fst irAllocaState) ++ (fst irStoreState)) putCounter putTable)
        return (list got)

      irAlloca :: CDeclaration -> [CDeclaration] -> GeneratorStateMonad [(Maybe IRLabel, IRInstruction)]
      irAlloca a [] = do
        got <- get
        return (list got)

      irAlloca a (b:bs) = do
        got <- get
        if (Map.notMember (getIdentifier b) (table got)) then do
            let instruction = generateIRAlloca (generateIRType a (getPointer b))
            let putTable = Map.insert (getIdentifier b) (IRLabelNumber (counter got), (generateIRType a (getPointer b))) (table got)
            put (GeneratorState ((list got) ++ [(Just (IRLabelNumber (counter got)), instruction)]) ((counter got) + 1) putTable)
          else do
            error ""
        irAlloca a bs

      irStore :: CDeclaration -> [CDeclaration] -> GeneratorStateMonad [(Maybe IRLabel, IRInstruction)]
      irStore a [] = do
        got <- get
        return (list got)

      irStore a (b:bs) = do
        got <- get
        let instruction = generateIRStore (generateIRType a (getPointer b)) (IRConstantValue (generateIRConstant (getConstant b) (generateIRType a (getPointer b))))  (fst ((table got) Map.! getIdentifier b))
        put (GeneratorState ((list got) ++ [(Nothing, instruction)]) (counter got) (table got))
        irStore a bs

      statementList (Just (CList a)) = a
      statementList Nothing = []

      statements :: [CStatement] -> GeneratorStateMonad [(Maybe IRLabel, IRInstruction)]
      statements [] = do
        got <- get
        return (list got)

      statements (a:as) = do
        statement a
        statements as

      statement :: CStatement -> GeneratorStateMonad [(Maybe IRLabel, IRInstruction)]
      statement (CCExpression (Just (CExpression []))) = do
        got <- get
        put (GeneratorState ((list got) ++ [(Nothing, IRRet Nothing)]) (counter got) (table got))
        return (list got)

      statement (CCExpression (Just (CExpression a))) = do
        got <- get
        let expressionsState = runState (expressions a) (GeneratorState [] (counter got) (table got))
        put (GeneratorState ((list got) ++ (fst expressionsState)) ((counter . snd) expressionsState) (table got))
        return (list got)

      statement (CReturn Nothing) = do
        got <- get
        put (GeneratorState ((list got) ++ [(Nothing, IRRet Nothing)]) (counter got) (table got))
        return (list got)

      statement (CReturn (Just a)) = do
        got <- get
        put (GeneratorState ((list got) ++ [(Nothing, IRRet (Just (IRConstantValue (generateIRConstant a c))))]) (counter got) (table got))
        return (list got)

      expressions :: [CExpression] -> GeneratorStateMonad [(Maybe IRLabel, IRInstruction)]
      expressions [] = do
        got <- get
        return (list got)

      expressions (a:as) = do
        expression a
        expressions as

      expression :: CExpression -> GeneratorStateMonad [(Maybe IRLabel, IRInstruction)]
      expression (CIdentifier a) = do
        got <- get
        let symbol = (table got) Map.! (identifier a)
        let instructions = [((Just (IRLabelNumber (counter got))), generateIRLoad (snd symbol) (IRLabelValue (fst symbol)))]
        put (GeneratorState ((list got) ++ instructions) ((counter got) + 1) (table got))
        return (instructions)

      expression (CProduct a b) = do
        got <- get
        let expr = runState (binaryExpression a b "*") (GeneratorState [] (counter got) (table got))
        put (GeneratorState ((list got) ++ (fst expr)) ((counter . snd) expr) ((table . snd) expr))
        return (fst expr)

      expression (CAssignment "=" (CIdentifier a) (CConstant b)) = do
        got <- get
        let symbol = (table got) Map.! (identifier a)
        let instructions = (list got) ++ [(Just (IRLabelNumber (counter got)), generateIRAlloca (snd symbol)), (Nothing, generateIRStore (snd symbol) (IRConstantValue (generateIRConstant (CConstant b) (snd symbol))) (IRLabelNumber (counter got))), (Nothing, generateIRStore (snd symbol) (IRLabelValue (IRLabelNumber (counter got))) (fst symbol))]
        put (GeneratorState ((list got) ++ instructions) ((counter got) + 1) (table got))
        return (instructions)

      expression (CAssignment "=" (CIdentifier a) b) = do
        got <- get
        let symbol = (table got) Map.! (identifier a)
        let expr = runState (expression b) (GeneratorState [] (counter got) (table got))
        let putCounter = (counter . snd) expr
        let instructions = (fst expr) ++ [(Nothing, generateIRStore (snd symbol) (IRLabelValue (IRLabelNumber (putCounter - 1))) (fst symbol))]
        put (GeneratorState (instructions) (putCounter) (table got))
        return (instructions)

      expression (CAssignment a (CIdentifier b) c) = do
        got <- get
        let symbol = (table got) Map.! (identifier b)
        let expr = runState (expression c) (GeneratorState [] (counter got) (table got))
        let other = (getType . snd . last . fst) expr
        let putCounter = (counter . snd) expr
        let instructions = (fst expr) ++ [(Just (IRLabelNumber putCounter), generateIRLoad (snd symbol) (IRLabelValue (fst symbol))), ((Just (IRLabelNumber (putCounter + 1))), (assignmentInstruction a other (snd symbol) (IRLabelValue (IRLabelNumber (putCounter - 1))) (IRLabelValue (IRLabelNumber putCounter)))), (Nothing, generateIRStore (snd symbol) (IRLabelValue (IRLabelNumber (putCounter + 1))) (fst symbol))]
        put (GeneratorState (instructions) (putCounter + 2) (table got))
        return (instructions)

      binaryExpression :: CExpression -> CExpression -> String -> GeneratorStateMonad [(Maybe IRLabel, IRInstruction)]
      binaryExpression a b c = do
        got <- get
        let firstExpr = runState (expression a) (GeneratorState [] (counter got) (table got))
        let firstType = ((getType . snd . last . fst) firstExpr)
        let firstCounter = (counter . snd) firstExpr
        let secondExpr = runState (expression b) (GeneratorState [] (firstCounter) (table got))
        let secondType = ((getType . snd . last . fst) secondExpr)
        let secondCounter = (counter . snd) secondExpr
        let instructions = (fst firstExpr) ++ (fst secondExpr) ++ [(Just (IRLabelNumber (secondCounter)), (binaryInstruction c firstType secondType) firstType (IRLabelValue (IRLabelNumber (firstCounter - 1))) (IRLabelValue (IRLabelNumber (secondCounter - 1))))]
        put (GeneratorState ((list got) ++ instructions) (secondCounter + 1) (table got))
        return (instructions)

      binaryInstruction a b c
        | a == "*" && (isIRInteger <$> (isSameType b c)) == Just True = IRMul
        | a == "*" && (isIRFloating <$> (isSameType b c)) == Just True = IRFmul
        | a == "/" && (isIRFloating <$> (isSameType b c)) == Just True = IRFdiv
        | a == "/" && isSameType b c == Just (IRInteger IRUnsigned) = IRUdiv
        | a == "/" && isSameType b c == Just (IRInteger IRSigned) = IRSdiv
        | a == "%" && isSameType b c /= Just (IRInteger IRUnsigned) = IRUrem
        | a == "%" && isSameType b c == Just (IRInteger IRSigned) = IRSrem
        | a == "%" && (isIRFloating <$> (isSameType b c)) == Just True = IRFrem
        | a == "+" && (isIRInteger <$> (isSameType b c)) == Just True = IRAdd
        | a == "+" && (isIRFloating <$> (isSameType b c)) == Just True = IRFadd
        | a == "-" && (isIRInteger <$> (isSameType b c)) == Just True = IRSub
        | a == "-" && (isIRFloating <$> (isSameType b c)) == Just True = IRFsub
        | a == "<<" && (isIRInteger <$> (isSameType b c)) == Just True = IRShl
        | a == ">>" && (isIRInteger <$> (isSameType b c)) == Just True = IRAshr
        | a == "&" && (isIRInteger <$> (isSameType b c)) == Just True = IRAnd
        | a == "^" && (isIRInteger <$> (isSameType b c)) == Just True = IRXor
        | a == "|" && (isIRInteger <$> (isSameType b c)) == Just True = IROr

      assignmentInstruction a b c d e
        | a == "<<=" || a == ">>=" = (binaryInstruction (init a) b c) c e d
        | otherwise = (binaryInstruction (init a) b c) c d e

  generateIRFunctionGlobal (CFunction (Just a) b _ c) = [IRFunctionGlobal functionType (name b) (map argument (argumentList b)) (generateIRBasicBlock c functionType)]
    where
      functionType = (generateIRType a (pointer b))
      name (CDeclarator _ (CDirectDeclaratorIdentifier (CIdentifier (CIdentifierToken a)))) = a
      name (CDeclarator _ (CDirectDeclaratorFunctionCall (CDirectDeclaratorIdentifier (CIdentifier (CIdentifierToken a))) _)) = a
      pointer (CDeclarator a _) = a
      argumentList (CDeclarator _ (CDirectDeclaratorFunctionCall _ [CParameterList a])) = a
      argument (CParameterDeclaration c d) = IRArgument (generateIRType c Nothing) (Just (name d))

  generateIRVariableGlobal (CExternalDeclaration (CDeclaration a (Just (CInitDeclaratorList b)))) = map variable b
    where
      variable (CDeclarator c (CDirectDeclaratorIdentifier (CIdentifier (CIdentifierToken d)))) = IRVariableGlobal d (generateIRType a c) (generateIRConstant (CConstant (CConstantToken (CIntegerConstant 0)))(generateIRType a c))
      variable (CInitDeclarator (CDeclarator c (CDirectDeclaratorIdentifier (CIdentifier (CIdentifierToken d)))) e) = IRVariableGlobal d (generateIRType a c) (generateIRConstant e (generateIRType a c))

  generateIRModule (CTranslationUnit a) = IRModule (concat (map cExternalDefinition a))
    where
      cExternalDefinition (CFunction c d e f) = generateIRFunctionGlobal (CFunction c d e f)
      cExternalDefinition (CExternalDeclaration c) = generateIRVariableGlobal (CExternalDeclaration c)

  generateIRBasicBlockCode a = concat (map irBasicBlockCode a)
    where
      irBasicBlockCode (IRBasicBlock _ a) = concat (map instruction a)
      instruction (Nothing, IRRet Nothing) = ["mov r0, r3", "bx lr"]
      instruction (Nothing, IRRet (Just a)) = ["mov r3, " ++ (value a), "mov r0, r3", "bx lr"]
      value (IRConstantValue (IRIntegerConstant a)) = show a
      value (IRConstantValue (IRFloatingConstant a)) = show a

  generateIRModuleCode (IRModule a) = intercalate "\n" (map (intercalate "\n" . irGlobalValueCode) a)
    where
      irGlobalValueCode (IRFunctionGlobal c d e f) = [d ++ ":"] ++ map ("\t" ++) (["push {r7}"] ++ generateIRBasicBlockCode f ++  ["bx lr"])
      irGlobalValueCode (IRVariableGlobal c d e) = [c ++ ":", "\t" ++ directive d ++ value e]
      directive (IRShortInteger _) = ".short "
      directive (IRInteger _) = ".int "
      directive (IRLongInteger _) = ".long "
      directive IRFloat = ".float "
      directive IRDouble = ".double "
      directive IRLongDouble = ".double "
      value (IRIntegerConstant a) = show a
      value (IRFloatingConstant a) = show a

  generate a = (generateIRModuleCode . generateIRModule) a
