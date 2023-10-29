module Generator where
  import Control.Monad.State
  import Data.Char (toLower, ord)
  import Data.List (intercalate)
  import Data.Map (Map)
  import qualified Data.Map as Map
  import Data.Set (fromList)
  import Syntax

  data GeneratorState = GeneratorState {list :: [(Maybe IRLabel, IRInstruction)], counter :: Integer, table :: Map String (IRLabel, IRType)}

  type GeneratorStateMonad = State GeneratorState

  getIdentifier (CDeclarator _ (CDirectDeclaratorIdentifier (CIdentifier (CIdentifierToken a)))) = a
  getIdentifier (CInitDeclarator (CDeclarator _ (CDirectDeclaratorIdentifier (CIdentifier (CIdentifierToken a)))) _) = a

  getPointer (CDeclarator a _) = a
  getPointer (CInitDeclarator a _) = getPointer a

  getConstant (CInitDeclarator _ a) = a
  getConstant (CDeclarator _ _) = CConstant (CConstantToken (CIntegerConstant 0 Nothing))

  getType (IRAdd a _ _) = a
  getType (IRFadd a _ _) = a
  getType (IRSub a _ _) = a
  getType (IRFsub a _ _) = a
  getType (IRMul a _ _) = a
  getType (IRFmul a _ _) = a
  getType (IRUdiv a _ _) = a
  getType (IRSdiv a _ _) = a
  getType (IRFdiv a _ _) = a
  getType (IRUrem a _ _) = a
  getType (IRSrem a _ _) = a
  getType (IRFrem a _ _) = a
  getType (IRShl a _ _) = a
  getType (IRLshr a _ _) = a
  getType (IRAshr a _ _) = a
  getType (IRAnd a _ _) = a
  getType (IROr a _ _) = a
  getType (IRXor a _ _) = a
  getType (IRLoad a _ _) = a
  getType (IRStore a _ _ _) = a
  getType (IRIcmp _ _ _ _) = IRInteger IRSigned
  getType (IRFcmp _ _ _ _) = IRInteger IRSigned
  getType (IRSext _ _ a) = a
  getType (IRFptrunc _ _ a) = a
  getType (IRFpext _ _ a) = a
  getType (IRFptoui _ _ a) = a
  getType (IRFptosi _ _ a) = a
  getType (IRUitofp _ _ a) = a
  getType (IRSitofp _ _ a) = a
  getType (IRPtrtoint _ _ a) = a
  getType (IRInttoptr _ _ a) = a
  getType (IRBitcast _ _ a) = a
  getType (IRAddrspacecast _ _ a) = a

  getOperator a
    | a == "=" = a
    | otherwise = init a

  isSameType a b
    | isIRShortInteger a && isIRShortInteger b || isIRInteger a && isIRInteger b || isIRLongInteger a && isIRLongInteger b || a == b = Just b
    | otherwise = Nothing

  isIRSigned a = a == IRShortInteger IRSigned || a == IRInteger IRSigned || a == IRLongInteger IRSigned

  isIRUnsigned a = a == IRShortInteger IRUnsigned || a == IRInteger IRUnsigned || a == IRLongInteger IRUnsigned

  isIRShortInteger a = a == IRShortInteger IRSigned || a == IRShortInteger IRUnsigned

  isIRInteger a = a == IRInteger IRSigned || a == IRInteger IRUnsigned

  isIRLongInteger a = a == IRLongInteger IRSigned || a == IRLongInteger IRUnsigned

  isIntegral a = isIRShortInteger a || isIRInteger a || isIRLongInteger a

  isFloating a = a == IRFloat || a == IRDouble || a == IRLongDouble

  typeFromCConstant (CFloatingConstant _ Nothing) = IRDouble
  typeFromCConstant (CFloatingConstant _ (Just a))
    | toLower a == 'f' = IRFloat
    | toLower a == 'l' = IRLongDouble

  typeFromCConstant (CIntegerConstant _ Nothing) = IRInteger IRSigned
  typeFromCConstant (CIntegerConstant _ (Just (a, Nothing)))
    | toLower a == 'u' = IRInteger IRUnsigned
    | toLower a == 'l' = IRLongInteger IRSigned

  typeFromCConstant (CIntegerConstant _ (Just (a, Just b))) = IRLongInteger IRUnsigned

  typeFromCSpecifiers a (Just (CPointer _)) = IRPointer (typeFromCSpecifiers a Nothing)

  typeFromCSpecifiers (CSpecifiers a) Nothing
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
      value (CConstant (CConstantToken (CFloatingConstant a _))) = a
      value (CConstant (CConstantToken (CIntegerConstant a _))) = fromIntegral a
      value (CConstant (CConstantToken (CCharacterConstant a))) = (fromIntegral . ord) a
      value (CExpression [a]) = value a
      value _ = 0

  generateIRAlloca a = IRAlloca a Nothing Nothing

  generateIRStore a b c = IRStore a b c Nothing

  generateIRLoad a b = IRLoad a b Nothing

  generateIRBasicBlock :: CStatement -> IRType -> GeneratorStateMonad IRBasicBlock
  generateIRBasicBlock (CCompound a b) c = do
    got <- get
    let dec = runState ((declarations . declarationList) a) got
    let stat = runState ((statements . statementList) b) (snd dec)
    return (IRBasicBlock (IRLabelNumber 0) (fst stat))
    where
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
        if Map.notMember (getIdentifier b) (table got) then do
            let instruction = generateIRAlloca (typeFromCSpecifiers a (getPointer b))
            let putTable = Map.insert (getIdentifier b) (IRLabelNumber (counter got), (typeFromCSpecifiers a (getPointer b))) (table got)
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
        let instruction = generateIRStore (typeFromCSpecifiers a (getPointer b)) (IRConstantValue (generateIRConstant (getConstant b) (typeFromCSpecifiers a (getPointer b))))  (fst ((table got) Map.! getIdentifier b))
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
      statement (CList a) = do
        got <- get
        let stat = runState (statements a) (GeneratorState [] (counter got) (table got))
        put (GeneratorState ((list got) ++ (fst stat)) ((counter . snd) stat) (table got))
        return (fst stat)

      statement (CCompound a Nothing) = return []

      statement (CCompound a (Just b)) = do
        got <- get
        let stat = runState (statement b) (GeneratorState [] (counter got) (table got))
        put (GeneratorState ((list got) ++ (fst stat)) ((counter . snd) stat) (table got))
        return (fst stat)

      statement (CCExpression (Just (CExpression []))) = do
        got <- get
        put (GeneratorState ((list got) ++ [(Nothing, IRTerminator (IRRet Nothing))]) (counter got) (table got))
        return (list got)

      statement (CCExpression (Just (CExpression a))) = do
        got <- get
        let expr = runState (expressions a) (GeneratorState [] (counter got) (table got))
        put (GeneratorState ((list got) ++ (fst expr)) ((counter . snd) expr) (table got))
        return (list got)

      statement (CReturn Nothing) = do
        got <- get
        put (GeneratorState ((list got) ++ [(Nothing, IRTerminator (IRRet Nothing))]) (counter got) (table got))
        return (list got)

      statement (CReturn (Just a)) = do
        got <- get
        put (GeneratorState ((list got) ++ [(Nothing, IRTerminator (IRRet (Just (IRConstantValue (generateIRConstant a c)))))]) (counter got) (table got))
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
        return instructions

      expression (CConstant a) = do
        got <- get
        let instructions = (list got) ++ [(Just (IRLabelNumber (counter got)), generateIRAlloca ((typeFromCConstant . constant) a)), (Nothing, generateIRStore ((typeFromCConstant . constant) a) (IRConstantValue (generateIRConstant (CConstant a) ((typeFromCConstant . constant) a))) (IRLabelNumber (counter got)))]
        put (GeneratorState ((list got) ++ instructions) ((counter got) + 1) (table got))
        return instructions

      expression (CBinary a b c) = do
        got <- get
        let expr = runState (binaryExpression b c a) (GeneratorState [] (counter got) (table got))
        let instructions = fst expr
        put (GeneratorState ((list got) ++ instructions) ((counter . snd) expr) ((table . snd) expr))
        return (fst expr)

      expression (CAssignment a (CIdentifier b) c) = do
        got <- get
        let symbol = (table got) Map.! (identifier b)
        let expr = runState (binaryExpression (CIdentifier b) c (getOperator a)) (GeneratorState [] (counter got) (table got))
        let instructions = (fst expr) ++ [(Nothing, generateIRStore (snd symbol) (IRLabelValue (IRLabelNumber (((counter . snd) expr) - 1))) (fst symbol))]
        put (GeneratorState ((list got) ++ instructions) ((counter . snd) expr) ((table . snd) expr))
        return instructions

      binaryExpression :: CExpression -> CExpression -> String -> GeneratorStateMonad [(Maybe IRLabel, IRInstruction)]
      binaryExpression a b c = do
        got <- get
        let firstExpr = runState (expression a) (GeneratorState [] (counter got) (table got))
        let firstType = (getType . snd . last . fst) firstExpr
        let firstCounter = (counter . snd) firstExpr
        if c == "=" then do
          let secondExpr = runState (expression b) (GeneratorState [] (counter got) (table got))
          let secondType = ((getType . snd . last . fst) secondExpr)
          let secondCounter = (counter . snd) secondExpr
          let castExpr = runState (castExpression (firstType, (IRLabelValue (IRLabelNumber (secondCounter - 1)))) (secondType, (IRLabelValue (IRLabelNumber (secondCounter - 1)))) False) (GeneratorState [] secondCounter (table got))
          let castCounter = (counter . snd) castExpr
          let instructions = (fst secondExpr) ++ (fst castExpr)
          put (GeneratorState ((list got) ++ instructions) (castCounter) (table got))
          return instructions
        else do
          let secondExpr = runState (expression b) (GeneratorState [] (firstCounter) (table got))
          let secondType = ((getType . snd . last . fst) secondExpr)
          let secondCounter = (counter . snd) secondExpr
          let castExpr = runState (castExpression (firstType, (IRLabelValue (IRLabelNumber (firstCounter - 1)))) (secondType, (IRLabelValue (IRLabelNumber (secondCounter - 1)))) True) (GeneratorState [] secondCounter (table got))
          let castType = (getType . snd . last) ((fst firstExpr) ++ (fst secondExpr) ++ (fst castExpr))
          let castCounter = (counter . snd) castExpr
          let bin = binaryExpression (firstType, firstCounter) (secondType, secondCounter) (castType, castCounter)
          let instructions = (fst firstExpr) ++ (fst secondExpr) ++ (fst castExpr) ++ [(Just (IRLabelNumber (castCounter)), bin)]
          put (GeneratorState ((list got) ++ instructions) (castCounter + 1) (table got))
          return instructions
        where
          binaryExpression d e f
            | fst d == fst f = (binaryInstruction c (fst d) (fst f)) (fst f) (IRLabelValue (IRLabelNumber ((snd d) - 1))) (IRLabelValue (IRLabelNumber ((snd f) - 1)))
            | fst e == fst f = (binaryInstruction c (fst e) (fst f)) (fst f) (IRLabelValue (IRLabelNumber ((snd f) - 1))) (IRLabelValue (IRLabelNumber ((snd e) - 1)))

      castExpression :: (IRType, IRValue) -> (IRType, IRValue) -> Bool -> GeneratorStateMonad [(Maybe IRLabel, IRInstruction)]
      castExpression a b c = do
        if isSameType (fst a) (fst b) == Nothing then do
          got <- get
          let instructions = [(Just (IRLabelNumber (counter got)), castInstruction a b c)]
          put (GeneratorState ((list got) ++ instructions) ((counter got) + 1) (table got))
          return instructions
        else
          return []

      castInstruction a b c
        | isIRShortInteger (fst a) && isIRInteger (fst b) && c = IRSext (fst a) (snd a) (fst b)
        | isIRShortInteger (fst a) && isIRInteger (fst b) && not c = IRTrunc (fst b) (snd b) (fst b)
        | isIRShortInteger (fst a) && isIRLongInteger (fst b) && c = IRSext (fst a) (snd a) (fst b)
        | isIRShortInteger (fst a) && isIRLongInteger (fst b) && not c = IRTrunc (fst b) (snd b) (fst b)
        | isIRInteger (fst a) && isIRShortInteger (fst b) && c = IRSext (fst b) (snd b) (fst a)
        | isIRInteger (fst a) && isIRShortInteger (fst b) && not c = IRTrunc (fst a) (snd a) (fst b)
        | isIRInteger (fst a) && isIRLongInteger (fst b) && c = IRSext (fst a) (snd a) (fst b)
        | isIRInteger (fst a) && isIRLongInteger (fst b) && not c = IRTrunc (fst b) (snd b) (fst a)
        | isIRLongInteger (fst a) && isIRShortInteger (fst b) && c = IRSext (fst b) (snd b) (fst a)
        | isIRLongInteger (fst a) && isIRShortInteger (fst b) && not c = IRTrunc (fst a) (snd a) (fst b)
        | isIRLongInteger (fst a) && isIRInteger (fst b) && c = IRSext (fst b) (snd b) (fst a)
        | isIRLongInteger (fst a) && isIRInteger (fst b) && not c = IRTrunc (fst a) (snd a) (fst b)
        | (fst a) == IRFloat && (fst b) == IRDouble && c = IRFpext (fst a) (snd a) (fst b)
        | (fst a) == IRFloat && (fst b) == IRDouble && not c = IRFptrunc (fst b) (snd b) (fst a)
        | (fst a) == IRFloat && (fst b) == IRLongDouble && c = IRFpext (fst a) (snd a) (fst b)
        | (fst a) == IRFloat && (fst b) == IRLongDouble && not c = IRFptrunc (fst b) (snd b) (fst a)
        | (fst a) == IRDouble && (fst b) == IRFloat && c = IRFpext (fst b) (snd b) (fst a)
        | (fst a) == IRDouble && (fst b) == IRFloat && not c = IRFptrunc (fst a) (snd a) (fst b)
        | (fst a) == IRDouble && (fst b) == IRLongDouble && c = IRFpext (fst a) (snd a) (fst b)
        | (fst a) == IRDouble && (fst b) == IRLongDouble && not c = IRFptrunc (fst b) (snd b) (fst a)
        | (fst a) == IRLongDouble && (fst b) == IRFloat && c = IRFpext (fst b) (snd b) (fst a)
        | (fst a) == IRLongDouble && (fst b) == IRFloat && not c = IRFptrunc (fst a) (snd a) (fst b)
        | (fst a) == IRLongDouble && (fst b) == IRDouble && c = IRFpext (fst b) (snd b) (fst a)
        | (fst a) == IRLongDouble && (fst b) == IRDouble && not c = IRFptrunc (fst a) (snd a) (fst b)
        | isIntegral (fst a) && isFloating (fst b) && c = IRSitofp (fst a) (snd a) (fst b)
        | isFloating (fst a) && isIntegral (fst b) && c = IRSitofp (fst b) (snd b) (fst a)
        | isIntegral (fst a) && isFloating (fst b) && not c = IRFptosi (fst b) (snd b) (fst a)
        | isFloating (fst a) && isIntegral (fst b) && not c = IRSitofp (fst b) (snd b) (fst a)

      binaryInstruction a b c
        | a == "*" && (isIntegral <$> (isSameType b c)) == Just True = IRMul
        | a == "*" && (isFloating <$> (isSameType b c)) == Just True = IRFmul
        | a == "/" && (isFloating <$> (isSameType b c)) == Just True = IRFdiv
        | a == "/" && (isIRUnsigned <$> (isSameType b c)) == Just True = IRUdiv
        | a == "/" && (isIRSigned <$> (isSameType b c)) == Just True = IRSdiv
        | a == "%" && (isIRUnsigned <$> (isSameType b c)) == Just True = IRUrem
        | a == "%" && (isIRSigned <$> (isSameType b c)) == Just True = IRSrem
        | a == "%" && (isFloating <$> (isSameType b c)) == Just True = IRFrem
        | a == "+" && (isIntegral <$> (isSameType b c)) == Just True = IRAdd
        | a == "+" && (isFloating <$> (isSameType b c)) == Just True = IRFadd
        | a == "-" && (isIntegral <$> (isSameType b c)) == Just True = IRSub
        | a == "-" && (isFloating <$> (isSameType b c)) == Just True = IRFsub
        | a == "<<" && (isIntegral <$> (isSameType b c)) == Just True = IRShl
        | a == ">>" && (isIntegral <$> (isSameType b c)) == Just True = IRAshr
        | a == "<" && (isIRUnsigned <$> (isSameType b c)) == Just True = IRIcmp IRIUlt
        | a == "<" && (isIRSigned <$> (isSameType b c)) == Just True = IRIcmp IRISlt
        | a == "<" && (isFloating <$> (isSameType b c)) == Just True = IRFcmp IRFOlt
        | a == ">" && (isIRUnsigned <$> (isSameType b c)) == Just True = IRIcmp IRIUgt
        | a == ">" && (isIRSigned <$> (isSameType b c)) == Just True = IRIcmp IRISgt
        | a == ">" && (isFloating <$> (isSameType b c)) == Just True = IRFcmp IRFOgt
        | a == "<=" && (isIRUnsigned <$> (isSameType b c)) == Just True = IRIcmp IRIUle
        | a == "<=" && (isIRSigned <$> (isSameType b c)) == Just True = IRIcmp IRISle
        | a == "<=" && (isFloating <$> (isSameType b c)) == Just True = IRFcmp IRFOle
        | a == ">=" && (isIRUnsigned <$> (isSameType b c)) == Just True = IRIcmp IRIUge
        | a == ">=" && (isIRSigned <$> (isSameType b c)) == Just True = IRIcmp IRISge
        | a == ">=" && (isFloating <$> (isSameType b c)) == Just True = IRFcmp IRFOge
        | a == "==" && (isIntegral <$> (isSameType b c)) == Just True = IRIcmp IRIEq
        | a == "==" && (isFloating <$> (isSameType b c)) == Just True = IRFcmp IRFOeq
        | a == "!=" && (isIntegral <$> (isSameType b c)) == Just True = IRIcmp IRINe
        | a == "!=" && (isFloating <$> (isSameType b c)) == Just True = IRFcmp IRFOne
        | a == "&" && (isIntegral <$> (isSameType b c)) == Just True = IRAnd
        | a == "^" && (isIntegral <$> (isSameType b c)) == Just True = IRXor
        | a == "|" && (isIntegral <$> (isSameType b c)) == Just True = IROr

      assignmentInstruction a b c d e
        | a == "<<=" || a == ">>=" = (binaryInstruction (getOperator a) b c) c e d
        | otherwise = (binaryInstruction (getOperator a) b c) c d e

  generateIRFunctionGlobal (CFunction (Just a) b _ c) = [IRFunctionGlobal functionType (name b) (map argument (argumentList b)) (evalState basicBlocks (GeneratorState [] 1 Map.empty))]
    where
      basicBlocks :: GeneratorStateMonad [IRBasicBlock]
      basicBlocks = do
        got <- get
        let block = runState (generateIRBasicBlock c functionType) got
        return ([fst block])

      functionType = (typeFromCSpecifiers a (pointer b))
      name (CDeclarator _ (CDirectDeclaratorIdentifier (CIdentifier (CIdentifierToken a)))) = a
      name (CDeclarator _ (CDirectDeclaratorFunctionCall (CDirectDeclaratorIdentifier (CIdentifier (CIdentifierToken a))) _)) = a
      pointer (CDeclarator a _) = a
      argumentList (CDeclarator _ (CDirectDeclaratorFunctionCall _ [CParameterList a])) = a
      argument (CParameterDeclaration c d) = IRArgument (typeFromCSpecifiers c Nothing) (Just (name d))

  generateIRVariableGlobal (CExternalDeclaration (CDeclaration a (Just (CInitDeclaratorList b)))) = map variable b
    where
      variable (CDeclarator c (CDirectDeclaratorIdentifier (CIdentifier (CIdentifierToken d)))) = IRVariableGlobal d (typeFromCSpecifiers a c) (generateIRConstant (CConstant (CConstantToken (CIntegerConstant 0 Nothing))) (typeFromCSpecifiers a c))
      variable (CInitDeclarator (CDeclarator c (CDirectDeclaratorIdentifier (CIdentifier (CIdentifierToken d)))) e) = IRVariableGlobal d (typeFromCSpecifiers a c) (generateIRConstant e (typeFromCSpecifiers a c))

  generateIRModule (CTranslationUnit a) = IRModule (concat (map cExternalDefinition a))
    where
      cExternalDefinition (CFunction c d e f) = generateIRFunctionGlobal (CFunction c d e f)
      cExternalDefinition (CExternalDeclaration c) = generateIRVariableGlobal (CExternalDeclaration c)

  generateIRBasicBlockCode a = concat (map irBasicBlockCode a)
    where
      irBasicBlockCode (IRBasicBlock _ a) = concat (map instruction a)
      instruction (Nothing, IRTerminator (IRRet Nothing)) = ["mov r0, r3", "bx lr"]
      instruction (Nothing, IRTerminator (IRRet (Just a))) = ["mov r3, " ++ (value a), "mov r0, r3", "bx lr"]
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
