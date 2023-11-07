module Generator where
  import Control.Monad.State
  import Data.Char (toLower, ord)
  import Data.List (intercalate)
  import Data.Map (Map)
  import qualified Data.Map as Map
  import Data.Set (fromList)
  import Syntax

  {-
    GeneratorState carries state between generators. A GeneratorState carries past basic blocks (blocks), present basic block
    instructions (list), an accumulator (counter) for unnamed temporaries, and a lookup table (table) which associates an
    identifier key with a label and type.
  -}
  data GeneratorState = GeneratorState {blocks :: [[(Maybe IRLabel, IRInstruction)]], list :: [(Maybe IRLabel, IRInstruction)], counter :: Integer, table :: Map String (IRLabel, IRType)}

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

  -- Allows usage of partial list operation functions.
  nonEmpty [] = [[]]
  nonEmpty a = a

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

  -- Set extensionality allows specifiers to be in any order.
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

  {-
    generateIRBasicBlocks is used for the generation of basic blocks, which are just instruction lists, from a function body,
    which is just a compound statement. It receives a compound statement (a), and the return type of the function (b).
  -}
  generateIRBasicBlocks :: CStatement -> IRType -> GeneratorStateMonad [IRBasicBlock]
  generateIRBasicBlocks a b = do
    got <- get
    let stat = execState (statement a) got
    return (fst (execState ((numberBlocks . blocks) stat) ([], 0)))
    where
      declarationList (Just (CDeclarationList a)) = a
      declarationList Nothing = []

      declarations :: [CDeclaration] -> GeneratorStateMonad ()
      declarations [] = return ()

      declarations (a:as) = do
        declaration a
        declarations as

      declaration :: CDeclaration -> GeneratorStateMonad ()
      declaration (CDeclaration d (Just (CInitDeclaratorList e))) = do
        got <- get
        let irAllocaState = execState (irAlloca d e) (GeneratorState [] [] (counter got) (table got))
        let putCounter = counter irAllocaState
        let putTable = table irAllocaState
        let irStoreState = execState (irStore d e) (GeneratorState [] [] putCounter putTable)
        put (GeneratorState (blocks got) ((list got) ++ (list irAllocaState) ++ (list irStoreState)) putCounter putTable)

      irAlloca :: CDeclaration -> [CDeclaration] -> GeneratorStateMonad ()
      irAlloca a [] = return ()

      irAlloca a (b:bs) = do
        got <- get
        if Map.notMember (getIdentifier b) (table got) then do
            let instruction = generateIRAlloca (typeFromCSpecifiers a (getPointer b))
            let putTable = Map.insert (getIdentifier b) (IRLabelNumber (counter got), (typeFromCSpecifiers a (getPointer b))) (table got)
            put (GeneratorState (blocks got) ((list got) ++ [(Just (IRLabelNumber (counter got)), instruction)]) ((counter got) + 1) putTable)
          else do
            error ""
        irAlloca a bs

      irStore :: CDeclaration -> [CDeclaration] -> GeneratorStateMonad ()
      irStore a [] = return ()

      irStore a (b:bs) = do
        got <- get
        let instruction = generateIRStore (typeFromCSpecifiers a (getPointer b)) (IRConstantValue (generateIRConstant (getConstant b) (typeFromCSpecifiers a (getPointer b)))) (fst ((table got) Map.! getIdentifier b))
        put (GeneratorState (blocks got) ((list got) ++ [(Nothing, instruction)]) (counter got) (table got))
        irStore a bs

      statementList (Just (CList a)) = a
      statementList Nothing = []

      statements :: [CStatement] -> GeneratorStateMonad ()
      statements [] = do
        got <- get
        put (GeneratorState ((blocks got) ++ [list got]) [] (counter got) (table got))

      statements (a:as) = do
        got <- get
        let stat = execState (statement a) (GeneratorState [] (list got) (counter got) (table got))
        put (GeneratorState ((blocks got) ++ (blocks stat)) (list stat) (counter stat) (table got))
        statements as

      statement :: CStatement -> GeneratorStateMonad ()
      statement (CList a) = do
        got <- get
        let stat = execState (statements a) (GeneratorState [] [] (counter got) (table got))
        put (GeneratorState (blocks got) ((list got) ++ (list stat)) (counter stat) (table got))

      statement (CCompound a b) = do
        got <- get
        let dec = execState ((declarations . declarationList) a) (GeneratorState [] (list got) (counter got) (table got))
        let stat = execState ((statements . statementList) b) (GeneratorState [] (list dec) (counter dec) (table dec))
        let instructions = (list got) ++ (list dec) ++ (list stat)
        put (GeneratorState ((blocks got) ++ (blocks stat)) [] (counter stat) (table stat))

      statement (CCExpression (Just (CExpression []))) = do
        got <- get
        put (GeneratorState (blocks got) ((list got) ++ [(Nothing, (IRRet Nothing))]) (counter got) (table got))

      statement (CCExpression (Just (CExpression a))) = do
        got <- get
        let expr = execState (expressions a) (GeneratorState [] [] (counter got) (table got))
        put (GeneratorState (blocks got) ((list got) ++ (list expr)) (counter expr) (table got))

      statement (CIf (CExpression a) b) = do
        got <- get
        let ifHead = execState (selectionHead (CExpression a) b) (GeneratorState [] (list got) (counter got) (table got))
        let ifBody = execState (statement b) (GeneratorState [] [] (counter ifHead) (table got))
        let ifBodyBlocks = ((nonEmpty . blocks) ifBody)
        let ifBr = (Nothing, IRBrUnconditional (IRLabelValue (IRLabelNumber (counter ifBody))))
        let putBlocks = (blocks ifHead) ++ (init ifBodyBlocks) ++ [(last ifBodyBlocks) ++ [ifBr]]
        put (GeneratorState putBlocks [] ((counter ifBody) + 1) (table got))

      statement (CIfElse (CExpression a) b c) = do
        got <- get
        let ifHead = execState (selectionHead (CExpression a) b) (GeneratorState [] (list got) (counter got) (table got))
        let ifBody = execState (statement b) (GeneratorState [] [] (counter ifHead) (table got))
        let ifBodyBlocks = ((nonEmpty . blocks) ifBody)
        let elseBody = execState (statement c) (GeneratorState [] [] ((counter ifBody) + 1) (table got))
        let elseBodyBlocks = ((nonEmpty . blocks) elseBody)
        let elseBr = (Nothing, IRBrUnconditional (IRLabelValue (IRLabelNumber (counter elseBody))))
        let putBlocks = (blocks ifHead) ++ (init ifBodyBlocks) ++ [(last ifBodyBlocks) ++ [elseBr]] ++ (init elseBodyBlocks) ++ [(last elseBodyBlocks) ++ [elseBr]]
        put (GeneratorState putBlocks [] ((counter elseBody) + 1) (table got))

      statement (CReturn Nothing) = do
        got <- get
        let instructions = (list got) ++ [(Nothing, IRRet Nothing)]
        put (GeneratorState (blocks got) instructions (counter got) (table got))

      statement (CReturn (Just a)) = do
        got <- get
        let instructions = (list got) ++ [(Nothing, IRRet (Just (IRConstantValue (generateIRConstant a b))))]
        put (GeneratorState (blocks got) instructions (counter got) (table got))

      {-
        All selection statements consist of an expression (a), and at least one body (b) which is a compound statement.
        Therefore, selectionHead generates this selection statement head so it can be used in other selection statement
        generators.
      -}
      selectionHead :: CExpression -> CStatement -> GeneratorStateMonad ()
      selectionHead (CExpression a) b = do
        got <- get
        let expr = execState (expressions a) (GeneratorState [] [] (counter got) (table got))
        let exprType = (getType . snd . last . list) expr
        let stat = execState (statement b) (GeneratorState [] [] ((counter expr) + 1) (table got))
        let cmp = ((Just (IRLabelNumber (counter expr))), comparisonInstruction (exprType, (counter expr)))
        let firstBr = (Nothing, IRBrConditional ((getType . snd) cmp) (IRLabelValue (IRLabelNumber (counter expr))) (IRLabelValue (IRLabelNumber ((counter expr) + 1))) (IRLabelValue (IRLabelNumber ((counter stat) + 1))))
        let putBlocks = [((list got) ++ (list expr) ++ [cmp] ++ [firstBr])]
        put (GeneratorState (putBlocks) [] ((counter expr) + 2) (table got))
        where
          -- Different comparison instructions are used depending on argument type.
          comparisonInstruction a
            | (isIntegral . fst) a = IRIcmp IRINe (fst a) (IRLabelValue (IRLabelNumber ((snd a) - 1))) (IRConstantValue (generateIRConstant (CConstant (CConstantToken (CIntegerConstant 0 Nothing))) (fst a)))
            | (isFloating . fst) a = IRFcmp IRFOne (fst a) (IRLabelValue (IRLabelNumber ((snd a) - 1))) (IRConstantValue (generateIRConstant (CConstant (CConstantToken (CIntegerConstant 0 Nothing))) (fst a)))

      {-
        The last instruction of a generated expression will contain the resulting value of that expression. This makes it
        easier to get the resulting type of an expression after generation.
      -}
      expressions :: [CExpression] -> GeneratorStateMonad ()
      expressions [] = return()

      expressions (a:as) = do
        expression a
        expressions as

      expression :: CExpression -> GeneratorStateMonad ()
      expression (CIdentifier a) = do
        got <- get
        let symbol = (table got) Map.! (identifier a)
        let instructions = (list got) ++ [((Just (IRLabelNumber (counter got))), generateIRLoad (snd symbol) (IRLabelValue (fst symbol)))]
        put (GeneratorState (blocks got) instructions ((counter got) + 1) (table got))

      expression (CConstant a) = do
        got <- get
        let instructions = (list got) ++ [(Just (IRLabelNumber (counter got)), generateIRAlloca ((typeFromCConstant . constant) a)), (Nothing, generateIRStore ((typeFromCConstant . constant) a) (IRConstantValue (generateIRConstant (CConstant a) ((typeFromCConstant . constant) a))) (IRLabelNumber (counter got)))]
        put (GeneratorState (blocks got) instructions ((counter got) + 1) (table got))

      expression (CBinary a b c) = do
        got <- get
        let expr = execState (binaryExpression b c a) (GeneratorState [] [] (counter got) (table got))
        let instructions = (list got) ++ (list expr)
        put (GeneratorState (blocks got) instructions (counter expr) (table expr))

      expression (CAssignment a (CIdentifier b) c) = do
        got <- get
        let symbol = (table got) Map.! (identifier b)
        let expr = execState (binaryExpression (CIdentifier b) c (getOperator a)) (GeneratorState [] [] (counter got) (table got))
        let instructions = (list got) ++ (list expr) ++ [(Nothing, generateIRStore (snd symbol) (IRLabelValue (IRLabelNumber ((counter expr) - 1))) (fst symbol))]
        put (GeneratorState (blocks got) instructions (counter expr) (table expr))

      binaryExpression :: CExpression -> CExpression -> String -> GeneratorStateMonad ()
      binaryExpression a b c = do
        got <- get
        let firstExpr = execState (expression a) (GeneratorState [] [] (counter got) (table got))
        let firstType = (getType . snd . last . list) firstExpr
        if c == "=" then do
          let secondExpr = execState (expression b) (GeneratorState [] [] (counter got) (table got))
          let secondType = ((getType . snd . last . list ) secondExpr)
          let castExpr = execState (castExpression (firstType, (IRLabelValue (IRLabelNumber ((counter secondExpr) - 1)))) (secondType, (IRLabelValue (IRLabelNumber ((counter secondExpr) - 1)))) False) (GeneratorState [] [] (counter secondExpr) (table got))
          let castCounter = counter castExpr
          let instructions = (list got) ++ (list secondExpr) ++ (list castExpr)
          put (GeneratorState (blocks got) instructions (castCounter) (table got))
        else do
          let secondExpr = execState (expression b) (GeneratorState [] [] ((counter firstExpr)) (table got))
          let secondType = ((getType . snd . last . list) secondExpr)
          let castExpr = execState (castExpression (firstType, (IRLabelValue (IRLabelNumber ((counter firstExpr) - 1)))) (secondType, (IRLabelValue (IRLabelNumber ((counter secondExpr) - 1)))) True) (GeneratorState [] [] (counter secondExpr) (table got))
          let castType = (getType . snd . last) ((list firstExpr) ++ (list secondExpr) ++ (list castExpr))
          let castCounter = counter castExpr
          let bin = binary (firstType, (counter firstExpr)) (secondType, (counter secondExpr)) (castType, castCounter)
          let instructions = (list got) ++ (list firstExpr) ++ (list secondExpr) ++ (list castExpr) ++ [(Just (IRLabelNumber (castCounter)), bin)]
          put (GeneratorState (blocks got) instructions (castCounter + 1) (table got))
        where
          {-
            Selects the expression which was not casted and uses it to generate the binary instruction. Ensures that the binary
            instruction is not generated with duplicate arguments.
          -}
          binary d e f
            | fst d == fst f = (binaryInstruction c (fst d) (fst f)) (fst f) (IRLabelValue (IRLabelNumber ((snd d) - 1))) (IRLabelValue (IRLabelNumber ((snd f) - 1)))
            | fst e == fst f = (binaryInstruction c (fst e) (fst f)) (fst f) (IRLabelValue (IRLabelNumber ((snd f) - 1))) (IRLabelValue (IRLabelNumber ((snd e) - 1)))

      castExpression :: (IRType, IRValue) -> (IRType, IRValue) -> Bool -> GeneratorStateMonad ()
      castExpression a b c
        | isSameType (fst a) (fst b) == Nothing = do
          got <- get
          let instructions = (list got) ++ [(Just (IRLabelNumber (counter got)), castInstruction a b c)]
          put (GeneratorState (blocks got) instructions ((counter got) + 1) (table got))
        | otherwise = return ()

      {-
        castInstruction receives two type-value pairs (a, and b), with different types and cast specifier (c). It will perform
        a cast operation to either a or b according to c such that a, and b both have the same type. If c is False, the cast is
        a downcast, and if c is True, the cast is an upcast.
      -}
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

      {-
        binaryInstruction receives a binary operation symbol as a string (a), and the types of the two arguments for the
        operation (a, and b) to ensure that they are the same type.
      -}
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

      {-
        It is hard to number blocks as they are being generated because there is not enough context. Therefore, block
        numbering is the last step. A block number can be calculated by counting the past numbered instructions in past blocks.
        The state tracks the list of blocks which have been numbered and the count of past numbered instructions.
      -}
      numberBlocks :: [[(Maybe IRLabel, IRInstruction)]] -> State ([IRBasicBlock], Integer) ()
      numberBlocks [] = return ()
      numberBlocks (a:as) = do
        got <- get
        let present = ((toInteger . length . filter numbered) a)
        let past = (snd got)
        put (((fst got) ++ [IRBasicBlock (IRLabelNumber past) a], present + past + 1))
        numberBlocks as
        where
          numbered (Nothing, _) = False
          numbered _ = True

  generateIRFunctionGlobal (CFunction (Just a) b _ c) = [IRFunctionGlobal functionType (name b) (map argument (argumentList b)) (evalState (generateIRBasicBlocks c functionType) (GeneratorState [] [] 1 Map.empty))]
    where
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
      instruction (Nothing, IRRet Nothing) = ["mov r0, r3", "bx lr"]
      instruction (Nothing, IRRet (Just a)) = ["mov r3, " ++ (value a), "mov r0, r3", "bx lr"]
      value (IRConstantValue (IRIntegerConstant a)) = show a
      value (IRConstantValue (IRFloatingConstant a)) = show a

  generateIRModuleCode (IRModule a) = intercalate "\n" (map (intercalate "\n" . irGlobalValueCode) a)
    where
      irGlobalValueCode (IRFunctionGlobal c d e f) = [d ++ ":"] ++ map ("\t" ++) (["push {r7}"] ++ generateIRBasicBlockCode f ++ ["bx lr"])
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
