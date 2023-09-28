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

  data GeneratorState = GeneratorState {list :: [(Maybe IRLabel, IRInstruction)], counter :: Integer, table :: Map String IRLabel}

  type GeneratorStateMonad = State GeneratorState

  generateIRType a (Just (CPointer _)) = IRPointer (generateIRType a Nothing)

  generateIRType (CSpecifiers a) Nothing
    | fromList a == fromList [CTypeSpecifier (CKeywordToken "void")] = IRVoid
    | fromList a == fromList [CTypeSpecifier (CKeywordToken "short")] = IRShortInteger
    | fromList a == fromList [CTypeSpecifier (CKeywordToken "signed"), CTypeSpecifier (CKeywordToken "short")] = IRShortInteger
    | fromList a == fromList [CTypeSpecifier (CKeywordToken "short"), CTypeSpecifier (CKeywordToken "int")] = IRShortInteger
    | fromList a == fromList [CTypeSpecifier (CKeywordToken "signed"), CTypeSpecifier (CKeywordToken "short"), CTypeSpecifier (CKeywordToken "int")] = IRShortInteger
    | fromList a == fromList [CTypeSpecifier (CKeywordToken "unsigned"), CTypeSpecifier (CKeywordToken "short")] = IRShortInteger
    | fromList a == fromList [CTypeSpecifier (CKeywordToken "unsigned"), CTypeSpecifier (CKeywordToken "short"), CTypeSpecifier (CKeywordToken "int")] = IRShortInteger
    | fromList a == fromList [CTypeSpecifier (CKeywordToken "int")] = IRInteger
    | fromList a == fromList [CTypeSpecifier (CKeywordToken "signed")] = IRInteger
    | fromList a == fromList [CTypeSpecifier (CKeywordToken "signed"), CTypeSpecifier (CKeywordToken "int")] = IRInteger
    | fromList a == fromList [CTypeSpecifier (CKeywordToken "unsigned")] = IRInteger
    | fromList a == fromList [CTypeSpecifier (CKeywordToken "unsigned"), CTypeSpecifier (CKeywordToken "int")] = IRInteger
    | fromList a == fromList [CTypeSpecifier (CKeywordToken "long")] = IRLongInteger
    | fromList a == fromList [CTypeSpecifier (CKeywordToken "signed"), CTypeSpecifier (CKeywordToken "long")] = IRLongInteger
    | fromList a == fromList [CTypeSpecifier (CKeywordToken "long"), CTypeSpecifier (CKeywordToken "int")] = IRLongInteger
    | fromList a == fromList [CTypeSpecifier (CKeywordToken "signed"), CTypeSpecifier (CKeywordToken "long"), CTypeSpecifier (CKeywordToken "int")] = IRLongInteger
    | fromList a == fromList [CTypeSpecifier (CKeywordToken "unsigned"), CTypeSpecifier (CKeywordToken "long")] = IRLongInteger
    | fromList a == fromList [CTypeSpecifier (CKeywordToken "unsigned"), CTypeSpecifier (CKeywordToken "long"), CTypeSpecifier (CKeywordToken "int")] = IRLongInteger
    | fromList a == fromList [CTypeSpecifier (CKeywordToken "char")] = IRInteger
    | fromList a == fromList [CTypeSpecifier (CKeywordToken "signed"), CTypeSpecifier (CKeywordToken "char")] = IRInteger
    | fromList a == fromList [CTypeSpecifier (CKeywordToken "unsigned"), CTypeSpecifier (CKeywordToken "char")] = IRInteger
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
      value _ = 0

  generateIRAlloca a b = IRAlloca (generateIRType a b) Nothing Nothing

  generateIRStore a b c d = IRStore (IRConstantValue (generateIRConstant d (generateIRType a b))) (generateIRType a b) c

  generateIRBasicBlock (CCompound a b) c = evalState irBasicBlock (GeneratorState [] 0 Map.empty)
    where
      irBasicBlock :: GeneratorStateMonad [IRBasicBlock]
      irBasicBlock = do
        getState <- get
        let declarationsState = runState ((declarations . declarationList) a) getState
        let statementsState = runState ((statements . statementList) b) getState
        return ([IRBasicBlock "" ((fst declarationsState) ++ (fst statementsState))])

      declarationList (Just (CDeclarationList a)) = a
      declarationList Nothing = []

      irAlloca :: CDeclaration -> [CDeclaration] -> GeneratorStateMonad [(Maybe IRLabel, IRInstruction)]
      irAlloca a [] = do
        getState <- get
        return (list getState)

      irAlloca a (b:bs) = do
        getState <- get
        if (Map.notMember (getIdentifier b) (table getState)) then do
            let instruction = generateIRAlloca a (getPointer b)
            let putTable = Map.insert (getIdentifier b) (IRLabelNumber (counter getState)) (table getState)
            put (GeneratorState ((list getState) ++ [(Just (IRLabelNumber (counter getState)), instruction)]) ((counter getState) + 1) putTable)
          else do
            error ""
        irAlloca a bs

      irStore :: CDeclaration -> [CDeclaration] -> GeneratorStateMonad [(Maybe IRLabel, IRInstruction)]
      irStore a [] = do
        getState <- get
        return (list getState)

      irStore a (b:bs) = do
        getState <- get
        let instruction = generateIRStore a (getPointer b) ((table getState) Map.! getIdentifier b) (getConstant b)
        put (GeneratorState ((list getState) ++ [(Nothing, instruction)]) (counter getState) (table getState))
        irStore a bs

      getIdentifier (CDeclarator _ (CDirectDeclaratorIdentifier (CIdentifier (CIdentifierToken a)))) = a
      getIdentifier (CInitDeclarator (CDeclarator _ (CDirectDeclaratorIdentifier (CIdentifier (CIdentifierToken a)))) _) = a

      getPointer (CDeclarator a _) = a
      getPointer (CInitDeclarator a _) = getPointer a

      getConstant (CInitDeclarator _ a) = a
      getConstant (CDeclarator _ _) = CConstant (CConstantToken (CIntegerConstant 0))

      declarations :: [CDeclaration] -> GeneratorStateMonad [(Maybe IRLabel, IRInstruction)]
      declarations [] = do
        getState <- get
        return (list getState)

      declarations (a:as) = do
        getState <- get
        case a of
          (CDeclaration d (Just (CInitDeclaratorList e))) -> do
            let irAllocaState = runState (irAlloca d e) (GeneratorState [] (counter getState) (table getState))
            let putCounter = (counter . snd) irAllocaState
            let putTable = (table . snd) irAllocaState
            let irStoreState = runState (irStore d e) (GeneratorState [] putCounter putTable)
            put (GeneratorState ((list getState) ++ (fst irAllocaState) ++ (fst irStoreState)) putCounter putTable)
        declarations as

      statementList (Just (CList a)) = a
      statementList Nothing = []

      statements :: [CStatement] -> GeneratorStateMonad [(Maybe IRLabel, IRInstruction)]
      statements [] = do
        getState <- get
        return (list getState)

      statements (a:as) = do
        getState <- get
        case a of
          (CCExpression (Just (CExpression []))) -> put (GeneratorState ((list getState) ++ [(Nothing, IRRet Nothing)]) (counter getState) (table getState))
          (CReturn Nothing) -> put (GeneratorState ((list getState) ++ [(Nothing, IRRet Nothing)]) (counter getState) (table getState))
          (CReturn (Just a)) -> put (GeneratorState ((list getState) ++ [(Nothing, IRRet (Just (IRConstantValue (generateIRConstant a c))))]) (counter getState) (table getState))
        statements as

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
      directive IRShortInteger = ".short "
      directive IRInteger = ".int "
      directive IRLongInteger = ".long "
      directive IRFloat = ".float "
      directive IRDouble = ".double "
      directive IRLongDouble = ".double "
      value (IRIntegerConstant a) = show a
      value (IRFloatingConstant a) = show a

  generate a = (generateIRModuleCode . generateIRModule) a
