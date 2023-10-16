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

  getType (IRLoad a _ _) = a

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
        gotState <- get
        let declarationsState = runState ((declarations . declarationList) a) gotState
        let statementsState = runState ((statements . statementList) b) (snd declarationsState)
        return [IRBasicBlock "" (fst statementsState)]

      declarationList (Just (CDeclarationList a)) = a
      declarationList Nothing = []

      declarations :: [CDeclaration] -> GeneratorStateMonad [(Maybe IRLabel, IRInstruction)]
      declarations [] = do
        gotState <- get
        return (list gotState)

      declarations (a:as) = do
        declaration a
        declarations as

      declaration :: CDeclaration -> GeneratorStateMonad [(Maybe IRLabel, IRInstruction)]
      declaration (CDeclaration d (Just (CInitDeclaratorList e))) = do
        gotState <- get
        let irAllocaState = runState (irAlloca d e) (GeneratorState [] (counter gotState) (table gotState))
        let putCounter = (counter . snd) irAllocaState
        let putTable = (table . snd) irAllocaState
        let irStoreState = runState (irStore d e) (GeneratorState [] putCounter putTable)
        put (GeneratorState ((list gotState) ++ (fst irAllocaState) ++ (fst irStoreState)) putCounter putTable)
        return (list gotState)

      irAlloca :: CDeclaration -> [CDeclaration] -> GeneratorStateMonad [(Maybe IRLabel, IRInstruction)]
      irAlloca a [] = do
        gotState <- get
        return (list gotState)

      irAlloca a (b:bs) = do
        gotState <- get
        if (Map.notMember (getIdentifier b) (table gotState)) then do
            let instruction = generateIRAlloca (generateIRType a (getPointer b))
            let putTable = Map.insert (getIdentifier b) (IRLabelNumber (counter gotState), (generateIRType a (getPointer b))) (table gotState)
            put (GeneratorState ((list gotState) ++ [(Just (IRLabelNumber (counter gotState)), instruction)]) ((counter gotState) + 1) putTable)
          else do
            error ""
        irAlloca a bs

      irStore :: CDeclaration -> [CDeclaration] -> GeneratorStateMonad [(Maybe IRLabel, IRInstruction)]
      irStore a [] = do
        gotState <- get
        return (list gotState)

      irStore a (b:bs) = do
        gotState <- get
        let instruction = generateIRStore (generateIRType a (getPointer b)) (IRConstantValue (generateIRConstant (getConstant b) (generateIRType a (getPointer b))))  (fst ((table gotState) Map.! getIdentifier b))
        put (GeneratorState ((list gotState) ++ [(Nothing, instruction)]) (counter gotState) (table gotState))
        irStore a bs

      statementList (Just (CList a)) = a
      statementList Nothing = []

      statements :: [CStatement] -> GeneratorStateMonad [(Maybe IRLabel, IRInstruction)]
      statements [] = do
        gotState <- get
        return (list gotState)

      statements (a:as) = do
        statement a
        statements as

      statement :: CStatement -> GeneratorStateMonad [(Maybe IRLabel, IRInstruction)]
      statement (CCExpression (Just (CExpression []))) = do
        gotState <- get
        put (GeneratorState ((list gotState) ++ [(Nothing, IRRet Nothing)]) (counter gotState) (table gotState))
        return (list gotState)

      statement (CCExpression (Just (CExpression a))) = do
        gotState <- get
        let expressionsState = runState (expressions a) (GeneratorState [] (counter gotState) (table gotState))
        put (GeneratorState ((list gotState) ++ (fst expressionsState)) ((counter . snd) expressionsState) (table gotState))
        return (list gotState)

      statement (CReturn Nothing) = do
        gotState <- get
        put (GeneratorState ((list gotState) ++ [(Nothing, IRRet Nothing)]) (counter gotState) (table gotState))
        return (list gotState)

      statement (CReturn (Just a)) = do
        gotState <- get
        put (GeneratorState ((list gotState) ++ [(Nothing, IRRet (Just (IRConstantValue (generateIRConstant a c))))]) (counter gotState) (table gotState))
        return (list gotState)

      expressions :: [CExpression] -> GeneratorStateMonad [(Maybe IRLabel, IRInstruction)]
      expressions [] = do
        gotState <- get
        return (list gotState)

      expressions (a:as) = do
        expression a
        expressions as

      expression :: CExpression -> GeneratorStateMonad [(Maybe IRLabel, IRInstruction)]
      expression (CIdentifier a) = do
        gotState <- get
        let symbol = (table gotState) Map.! (identifier a)
        let instructions = [((Just (IRLabelNumber (counter gotState))), generateIRLoad (snd symbol) (IRLabelValue (fst symbol)))]
        put (GeneratorState ((list gotState) ++ instructions) ((counter gotState) + 1) (table gotState))
        return (instructions)

      expression (CAssignment "=" (CIdentifier a) (CConstant b)) = do
        gotState <- get
        let symbol = (table gotState) Map.! (identifier a)
        let instructions = (list gotState) ++ [(Just (IRLabelNumber (counter gotState)), generateIRAlloca (snd symbol)), (Nothing, generateIRStore (snd symbol) (IRConstantValue (generateIRConstant (CConstant b) (snd symbol))) (IRLabelNumber (counter gotState))), (Nothing, generateIRStore (snd symbol) (IRLabelValue (IRLabelNumber (counter gotState))) (fst symbol))]
        put (GeneratorState ((list gotState) ++ instructions) ((counter gotState) + 1) (table gotState))
        return (instructions)

      expression (CAssignment "=" (CIdentifier a) b) = do
        gotState <- get
        let symbol = (table gotState) Map.! (identifier a)
        let expressionState = runState (expression b) (GeneratorState [] (counter gotState) (table gotState))
        let putCounter = (counter . snd) expressionState
        let instructions = (fst expressionState) ++ [(Nothing, generateIRStore (snd symbol) (IRLabelValue (IRLabelNumber (putCounter - 1))) (fst symbol))]
        put (GeneratorState (instructions) (putCounter) (table gotState))
        return (instructions)

      expression (CAssignment a (CIdentifier b) c) = do
        gotState <- get
        let symbol = (table gotState) Map.! (identifier b)
        let expressionState = runState (expression c) (GeneratorState [] (counter gotState) (table gotState))
        let other = (getType . snd . last . fst) expressionState
        let putCounter = (counter . snd) expressionState
        let instructions = (fst expressionState) ++ [(Just (IRLabelNumber putCounter), generateIRLoad (snd symbol) (IRLabelValue (fst symbol))), ((Just (IRLabelNumber (putCounter + 1))), (binaryOperator a other (snd symbol) (IRLabelValue (IRLabelNumber (putCounter - 1))) (IRLabelValue (IRLabelNumber putCounter)))), (Nothing, generateIRStore (snd symbol) (IRLabelValue (IRLabelNumber (putCounter + 1))) (fst symbol))]
        put (GeneratorState (instructions) (putCounter + 2) (table gotState))
        return (instructions)

      binaryOperator a b c d e
        | a == "<<=" || a == ">>=" = instruction c e d
        | otherwise =  instruction c d e
        where
          instruction
            | a == "*=" && b == IRFloat = IRFmul
            | a == "*=" = IRMul
            | a == "/=" && b == IRFloat = IRFdiv
            | a == "/=" && b == IRInteger IRSigned = IRSdiv
            | a == "/=" = IRUdiv
            | a == "%=" && b == IRFloat = IRFrem
            | a == "%=" && b == IRInteger IRSigned = IRSrem
            | a == "%=" = IRUrem
            | a == "+=" && b == IRFloat = IRFadd
            | a == "+=" = IRAdd
            | a == "-=" && b == IRFloat = IRFsub
            | a == "-=" = IRSub
            | a == "<<=" && isIRInteger c && isIRInteger b = IRShl
            | a == ">>=" && isIRInteger c && isIRInteger b = IRAshr
            | a == "&=" && isIRInteger c && isIRInteger b = IRAnd
            | a == "^=" && isIRInteger c && isIRInteger b = IRXor
            | a == "|=" && isIRInteger c && isIRInteger b = IROr

          isIRInteger a = a == IRInteger IRSigned || a == IRInteger IRUnsigned

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
