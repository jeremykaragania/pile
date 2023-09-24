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

  generateIRType (CSpecifiers [CTypeSpecifier (CKeywordToken a)]) (Just (CPointer _)) = IRPointer (generateIRType (CSpecifiers [CTypeSpecifier (CKeywordToken a)]) Nothing)

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

  generateIRBasicBlock (CCompound a b) c = [IRBasicBlock "" ((evalState ((declarations . declarationList) a) (GeneratorState [] 0 Map.empty)) ++ statements)]
    where
      declarationList (Just (CDeclarationList a)) = a
      declarationList Nothing = []

      irAlloca :: CDeclaration -> [CDeclaration] -> GeneratorStateMonad [(Maybe IRLabel, IRInstruction)]
      irAlloca a [] = do
        generatorState <- get
        return (list generatorState)

      irAlloca a (b:bs) = do
        generatorState <- get
        let listState = list generatorState
        let counterState = counter generatorState
        let tableState = table generatorState
        case b of
          (CDeclarator c (CDirectDeclaratorIdentifier (CIdentifier (CIdentifierToken d)))) -> put (GeneratorState (listState ++ [(Just (IRLabelNumber counterState), IRAlloca (generateIRType a c) Nothing Nothing)]) (counterState + 1) tableState)
          (CInitDeclarator (CDeclarator c (CDirectDeclaratorIdentifier (CIdentifier (CIdentifierToken d)))) e) -> put (GeneratorState (listState ++ [(Just (IRLabelNumber counterState), IRAlloca (generateIRType a c) Nothing Nothing)]) (counterState + 1) tableState)
        irAlloca a bs

      irStore :: CDeclaration -> [CDeclaration] -> GeneratorStateMonad [(Maybe IRLabel, IRInstruction)]
      irStore a [] = do
        generatorState <- get
        return (list generatorState)

      irStore a (b:bs) = do
        generatorState <- get
        let listState = list generatorState
        let counterState = counter generatorState
        let tableState = table generatorState
        case b of
          (CDeclarator c (CDirectDeclaratorIdentifier (CIdentifier (CIdentifierToken d)))) -> put (GeneratorState (listState ++ [(Nothing, IRStore (IRConstantValue (generateIRConstant (CConstant (CConstantToken (CIntegerConstant 0))) (generateIRType a c))) (IRConstantValue IRNullPointerConstant) Nothing)]) counterState tableState)
          (CInitDeclarator (CDeclarator c (CDirectDeclaratorIdentifier (CIdentifier (CIdentifierToken d)))) e) -> put (GeneratorState (listState ++ [(Nothing, IRStore (IRConstantValue (generateIRConstant e (generateIRType a c))) (IRConstantValue IRNullPointerConstant) Nothing)]) counterState tableState)
        irStore a bs

      declarations :: [CDeclaration] -> GeneratorStateMonad [(Maybe IRLabel, IRInstruction)]
      declarations [] = do
        generatorState <- get
        return (list generatorState)

      declarations (a:as) = do
        generatorState <- get
        let listState = list generatorState
        let counterState = counter generatorState
        let tableState = table generatorState
        case a of
          (CDeclaration d (Just (CInitDeclaratorList e))) -> do
            let irAllocaState = runState (irAlloca d e) (GeneratorState [] counterState tableState)
            let counterState = (counter . snd) irAllocaState
            let irStoreState = runState (irStore d e) (GeneratorState [] counterState tableState)
            put (GeneratorState (listState ++ (fst irAllocaState) ++ (fst irStoreState)) counterState tableState)
        declarations as

      statementList (Just (CList a)) = a
      statementList Nothing = []
      statement (CReturn Nothing) = IRRet Nothing
      statement (CReturn (Just a)) = IRRet (Just (IRConstantValue (generateIRConstant a c)))
      statements = zip (repeat Nothing) (map statement (statementList b))

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
