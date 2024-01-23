module Generator where
  import Control.Monad.State
  import Data.Char (toLower, ord)
  import Data.List (intercalate)
  import Data.Map (Map)
  import qualified Data.Map as Map
  import Data.Set (fromList)
  import Syntax

  {-
    GeneratorState carries state between generators. A GeneratorState carries past basic blocks (blocks), an accumulator
    (counter) for unnamed temporaries, a lookup table (table) which associates an identifier key with a label and type, and a
    context for break and continue statements (context) which carries the integer label number for the branch.
  -}
  data GeneratorState = GeneratorState {blocks :: [[(Maybe IRLabel, IRInstruction)]], counter :: Integer, table :: Map String (IRLabel, IRType), context :: (Maybe Integer)}

  setBlocks a (GeneratorState _ b c d) = GeneratorState a b c d

  setCounter a (GeneratorState b c d e) = GeneratorState b (a c) d e

  setTable a (GeneratorState b c _ d) = GeneratorState b c a d

  setContext a (GeneratorState b c d _) = GeneratorState b c d a

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
  getType (IRAlloca a) = a
  getType (IRLoad a _) = a
  getType (IRStore a _ _) = a
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

  statementFromCCompound(CCompound a b) = b

  generateIRConstant a b
    | b == IRFloat || b == IRDouble || b == IRLongDouble = IRFloatingConstant (value a)
    | otherwise = IRIntegerConstant ((floor . value) a)
    where
      value (CConstant (CConstantToken (CFloatingConstant a _))) = a
      value (CConstant (CConstantToken (CIntegerConstant a _))) = fromIntegral a
      value (CConstant (CConstantToken (CCharacterConstant a))) = (fromIntegral . ord) a
      value (CExpression [a]) = value a
      value _ = 0

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
        got <- get
        let dec = execState (declaration a) got
        put dec
        declarations as

      declaration :: CDeclaration -> GeneratorStateMonad ()
      declaration (CDeclaration d (Just (CInitDeclaratorList e))) = do
        got <- get
        let dec = execState (initDeclarators d e) got
        put dec

      declaration (CInitDeclarator _ b) = do
        got <- get
        let expr = execState (expression b) got
        let newBlocks = appendBlocks (blocks got) (blocks expr)
        put ((setBlocks newBlocks) expr)

      declaration (CDeclarator _ b) = return ()

      newAlloca :: IRType -> GeneratorStateMonad ()
      newAlloca a = do
        got <- get
        let alloca = [[(Just (IRLabelNumber (counter got)), IRAlloca a)]]
        let newBlocks = appendBlocks alloca (blocks got)
        put ((setBlocks newBlocks . setCounter (+1)) got)

      initDeclarators :: CDeclaration -> [CDeclaration] -> GeneratorStateMonad ()
      initDeclarators a [] = return ()

      initDeclarators a (b:bs) = do
        got <- get
        if Map.notMember (getIdentifier b) (table got) then do
            let firstType = (typeFromCSpecifiers a (getPointer b))
            let alloca = execState (newAlloca firstType) got
            let newTable = Map.insert (getIdentifier b) (IRLabelNumber (counter got), (typeFromCSpecifiers a (getPointer b))) (table got)
            let dec = execState (declaration b) alloca
            let secondType = (getType . snd . last . concat . blocks) dec
            let castExpr = execState (castExpression (firstType, (IRLabelValue (IRLabelNumber (counter got)))) (secondType, (IRLabelValue (IRLabelNumber (counter dec - 1)))) False) (setBlocks [[]] dec)
            let store = [[(Nothing, IRStore (typeFromCSpecifiers a (getPointer b)) (IRLabelValue (IRLabelNumber (counter castExpr - 1))) (IRLabelNumber (counter got)))]]
            let storeBlocks = appendBlocks (blocks dec) (appendBlocks (blocks castExpr) store)
            put ((setBlocks storeBlocks . setTable newTable) castExpr)
        else error ""
        initDeclarators a bs

      statementList (Just (CList a)) = a
      statementList Nothing = []

      statements :: [CStatement] -> GeneratorStateMonad ()
      statements [] = return ()

      statements (a:as) = do
        got <- get
        let stat = execState (statement a) (setBlocks [[]] got)
        let newBlocks = (appendBlocks (blocks got) (blocks stat))
        put (setBlocks newBlocks stat)
        statements as

      statement :: CStatement -> GeneratorStateMonad ()
      statement (CLabeledCase a b) = do
        labeled b

      statement (CLabeledDefault a) = do
        labeled a

      statement (CList a) = do
        got <- get
        let stat = execState (statements a) (setBlocks [[]] got)
        put stat

      statement (CCompound a b) = do
        got <- get
        let dec = execState ((declarations . declarationList) a) got
        let stat = execState ((statements . statementList) b) dec
        put stat

      statement (CCExpression (Just (CExpression []))) = do
        got <- get
        put got

      statement (CCExpression (Just (CExpression a))) = do
        got <- get
        let expr = execState (expressions a) (setBlocks [[]] got)
        put expr

      statement (CIf a@(CExpression _) b) = do
        got <- get
        let ifHead = execState (selectionHead a (compound b)) (setBlocks [[]] got)
        let ifBody = execState ((statement . compound) b) (setBlocks [[]] ifHead)
        let ifBr = [(Nothing, IRBrUnconditional (IRLabelValue (IRLabelNumber ((counter ifBody)))))]
        let newBlocks = (blocks ifHead) ++ appendBlocks (blocks ifBody) [ifBr, []]
        put ((setBlocks newBlocks . setCounter (+1)) ifBody)

      statement (CIfElse a@(CExpression _) b c) = do
        got <- get
        let ifHead = execState (selectionHead a (compound b)) (setBlocks [[]] got)
        let ifBody = execState ((statement . compound) b) (setBlocks [[]] ifHead)
        let elseBody = execState ((statement . compound) c) ((setBlocks [[]] . setCounter (+1)) ifBody)
        let elseBr = [(Nothing, IRBrUnconditional (IRLabelValue (IRLabelNumber ((counter elseBody)))))]
        let newBlocks = (blocks ifHead) ++ appendBlocks (blocks ifBody) (appendBlocks [elseBr, []] (appendBlocks (blocks elseBody) [elseBr, []]))
        put ((setBlocks newBlocks . setCounter (+1)) elseBody)

      statement (CSwitch (CExpression a) b) = do
        got <- get
        let splitStat = (CCompound Nothing . Just . CList . splitLabeled . statementList . statementFromCCompound . compound) b
        let expr = execState (expressions a) (setBlocks [[]] got)
        let exprType = (getType . snd . last . concat . blocks) expr
        if (isIntegral exprType) then do
          let stat = execState ((switchStatement . splitLabeled . statementList . statementFromCCompound . compound) b) ((setBlocks [[], []] . setCounter (+1) . setContext (Just ((counter . fst) stat)) . fst) stat, ([(counter . fst) stat], []))
          let switch = [(Nothing, IRSwitch exprType (IRLabelNumber ((fromIntegral . length) ((snd . snd) stat))) (IRLabelNumber (labeledDefault ((fst . snd) stat))) ((snd . snd) stat))]
          let switchBr = [(Nothing, IRBrUnconditional (IRLabelValue (IRLabelNumber (((counter . fst) stat)))))]
          let newBlocks = appendBlocks (blocks expr) (appendBlocks [switch] (appendBlocks ((blocks . fst) stat) [switchBr, []]))
          put ((setBlocks newBlocks . setCounter (+1) . setContext Nothing . fst) stat)
        else error ""
        where
          splitLabeled [] = []
          splitLabeled (a:as)
            | isLabeled a  = a:as
            | otherwise = splitLabeled as
          isLabeled (CLabeledCase _ _) = True
          isLabeled (CLabeledDefault _) = True
          isLabeled _ = False
          -- Decides whether to use the explicit (a) or implicit (b) default label.
          labeledDefault [a] = a
          labeledDefault [a, b] = b
          labeledDefault _ = error ""

      statement (CReturn Nothing) = do
        got <- get
        let newBlocks = [[(Nothing, IRRet Nothing)]]
        put (setBlocks newBlocks got)

      statement (CReturn (Just a)) = do
        got <- get
        let newBlocks = [[(Nothing, IRRet (Just (IRConstantValue (generateIRConstant a b))))]]
        put (setBlocks newBlocks got)

      statement (CBreak) = do
        got <- get
        case (context got) of
          Just a -> do
            let newBlocks = [[(Nothing, IRBrUnconditional (IRLabelValue (IRLabelNumber (a))))]]
            put (setBlocks newBlocks got)
          otherwise -> error ""

      labeled :: CStatement -> GeneratorStateMonad ()
      labeled a = do
        got <- get
        let stat = execState ((statement . compound) a) (setBlocks [[]] got)
        let switchBr = [(Nothing, IRBrUnconditional (IRLabelValue (IRLabelNumber ((counter stat)))))]
        let newBlocks = appendBlocks (blocks stat) [switchBr, []]
        put ((setBlocks newBlocks . setCounter (+1)) stat)

      switchStatement :: [CStatement] -> State (GeneratorState, ([Integer], [(IRType, IRConstant, IRLabel)])) ()
      switchStatement [] = return ()

      switchStatement (a:as) = do
        got <- get
        let stat = execState (statement a) (GeneratorState [] ((counter . fst) got) ((table . fst) got) ((context . fst) got))
        let newBlocks = appendBlocks ((blocks . fst) got) (blocks stat)
        case a of
          CLabeledCase a _ -> do
            let switchConstant = (constant . token) a
            let switchType = typeFromCConstant switchConstant
            put ((setBlocks newBlocks stat, ((fst . snd) got, (snd . snd) got ++ [(switchType, generateIRConstant a switchType, IRLabelNumber ((counter . fst) got - 1))])))
            switchStatement as
          CLabeledDefault _ -> do
            put ((setBlocks newBlocks stat, ((fst . snd) got ++ [(counter . fst) got - 1], (snd . snd) got)))
            switchStatement as
          otherwise -> do
            put ((setBlocks newBlocks stat, ((fst . snd) got, (snd . snd) got)))
            switchStatement as

      {-
        All selection statements consist of an expression (a), and at least one body (b) which is a compound statement.
        Therefore, selectionHead generates this selection statement head so it can be used in other selection statement
        generators.
      -}
      selectionHead :: CExpression -> CStatement -> GeneratorStateMonad ()
      selectionHead (CExpression a) b = do
        got <- get
        let expr = execState (expressions a) (setBlocks [[]] got)
        let exprType = (getType . snd . last . concat . blocks) expr
        let cmp = ((Just (IRLabelNumber (counter expr))), comparisonInstruction (exprType, (counter expr)))
        let stat = execState (statement b) ((setBlocks [[]] . setCounter (+1)) expr)
        let firstBr = (Nothing, IRBrConditional ((getType . snd) cmp) (IRLabelValue (IRLabelNumber (counter expr))) (IRLabelValue (IRLabelNumber (counter expr + 1))) (IRLabelValue (IRLabelNumber (counter stat + 1))))
        let newBlocks = appendBlocks (blocks expr) (appendBlocks [[cmp]] [[firstBr]])
        put ((setBlocks newBlocks . setCounter (+2)) expr)
        where
          -- Different comparison instructions are used depending on argument type.
          comparisonInstruction a
            | (isIntegral . fst) a = IRIcmp IRINe (fst a) (IRLabelValue (IRLabelNumber (snd a - 1))) (IRConstantValue (generateIRConstant (CConstant (CConstantToken (CIntegerConstant 0 Nothing))) (fst a)))
            | (isFloating . fst) a = IRFcmp IRFOne (fst a) (IRLabelValue (IRLabelNumber (snd a - 1))) (IRConstantValue (generateIRConstant (CConstant (CConstantToken (CIntegerConstant 0 Nothing))) (fst a)))

      {-
        The last instruction of a generated expression will contain the resulting value of that expression. This makes it
        easier to get the resulting type of an expression after generation.
      -}
      expressions :: [CExpression] -> GeneratorStateMonad ()
      expressions [] = return ()

      expressions (a:as) = do
        expression a
        expressions as

      expression :: CExpression -> GeneratorStateMonad ()
      expression (CIdentifier a) = do
        got <- get
        let symbol = (table got) Map.! (identifier a)
        let newBlocks = [[((Just (IRLabelNumber (counter got))), IRLoad (snd symbol) (IRLabelValue (fst symbol)))]]
        put ((setBlocks newBlocks . setCounter (+1)) got)

      expression a@(CConstant b) = do
        got <- get
        let newBlocks = [[(Just (IRLabelNumber (counter got)), IRAlloca ((typeFromCConstant . constant) b)), (Nothing, IRStore ((typeFromCConstant . constant) b) (IRConstantValue (generateIRConstant a ((typeFromCConstant . constant) b))) (IRLabelNumber (counter got)))]]
        put ((setBlocks newBlocks . setCounter (+1)) got)

      expression (CBinary a b c) = do
        got <- get
        let expr = execState (binaryExpression b c a) (setBlocks [[]] got)
        put expr

      expression (CAssignment a b@(CIdentifier c) d) = do
        got <- get
        let symbol = (table got) Map.! (identifier c)
        let expr = execState (binaryExpression b d (getOperator a)) (setBlocks [[]] got)
        let newBlocks = appendBlocks (blocks expr) [[(Nothing, IRStore (snd symbol) (IRLabelValue (IRLabelNumber (counter expr - 1))) (fst symbol))]]
        put (setBlocks newBlocks expr)

      binaryExpression :: CExpression -> CExpression -> String -> GeneratorStateMonad ()
      binaryExpression a b c = do
        got <- get
        let firstExpr = execState (expression a) (setBlocks [[]] got)
        let firstType = (getType . snd . last . concat . blocks) firstExpr
        if c == "=" then do
          let secondExpr = execState (expression b) (setBlocks [[]] got)
          let secondType = (getType . snd . last . concat . blocks) secondExpr
          let castExpr = execState (castExpression (firstType, (IRLabelValue (IRLabelNumber (counter secondExpr - 1)))) (secondType, (IRLabelValue (IRLabelNumber (counter secondExpr - 1)))) False) (setBlocks [[]] secondExpr)
          let newBlocks = appendBlocks (blocks secondExpr) (blocks castExpr)
          put ((setBlocks newBlocks) castExpr)
        else do
          let secondExpr = execState (expression b) (setBlocks [[]] firstExpr)
          let secondType = (getType . snd . last . concat . blocks) secondExpr
          let castExpr = execState (castExpression (firstType, (IRLabelValue (IRLabelNumber (counter firstExpr - 1)))) (secondType, (IRLabelValue (IRLabelNumber (counter secondExpr - 1)))) True) (setBlocks [[]] secondExpr)
          let initBlocks = (appendBlocks (blocks firstExpr) (appendBlocks (blocks secondExpr) (blocks castExpr)))
          let castType = (getType . snd . last . concat) initBlocks
          let bin = binary (firstType, (counter firstExpr)) (secondType, (counter secondExpr)) (castType, (counter castExpr))
          let newBlocks = appendBlocks initBlocks [[(Just (IRLabelNumber (counter castExpr)), bin)]]
          put ((setBlocks newBlocks . setCounter (+1)) castExpr)
        where
          {-
            Selects the expression which was not casted and uses it to generate the binary instruction. Ensures that the binary
            instruction is not generated with duplicate arguments.
          -}
          binary d e f
            | fst d == fst f = (binaryInstruction c (fst f)) (fst f) (IRLabelValue (IRLabelNumber (snd d - 1))) (IRLabelValue (IRLabelNumber (snd f - 1)))
            | fst e == fst f = (binaryInstruction c (fst f)) (fst f) (IRLabelValue (IRLabelNumber (snd f - 1))) (IRLabelValue (IRLabelNumber (snd e - 1)))

      castExpression :: (IRType, IRValue) -> (IRType, IRValue) -> Bool -> GeneratorStateMonad ()
      castExpression a b c
        | isSameType (fst a) (fst b) == Nothing = do
          got <- get
          let newBlocks = [[(Just (IRLabelNumber (counter got)), castInstruction a b c)]]
          put ((setBlocks newBlocks . setCounter (+1)) got)
        | otherwise = return ()

      {-
        castInstruction receives two type-value pairs (a, and b), with different types and cast specifier (c). It will perform
        a cast operation to either a or b according to c such that a, and b both have the same type. If c is False, the cast is
        a downcast, and if c is True, the cast is an upcast.
      -}
      castInstruction (a, b) (c, d) e
        | isIRShortInteger a && isIRInteger c && e = IRSext a b c
        | isIRShortInteger a && isIRInteger c && not e = IRTrunc c d c
        | isIRShortInteger a && isIRLongInteger c && e = IRSext a b c
        | isIRShortInteger a && isIRLongInteger c && not e = IRTrunc c d c
        | isIRInteger a && isIRShortInteger c && e = IRSext c d a
        | isIRInteger a && isIRShortInteger c && not e = IRTrunc a b c
        | isIRInteger a && isIRLongInteger c && e = IRSext a b c
        | isIRInteger a && isIRLongInteger c && not e = IRTrunc c d a
        | isIRLongInteger a && isIRShortInteger c && e = IRSext c d a
        | isIRLongInteger a && isIRShortInteger c && not e = IRTrunc a b c
        | isIRLongInteger a && isIRInteger c && e = IRSext c d a
        | isIRLongInteger a && isIRInteger c && not e = IRTrunc a b c
        | a == IRFloat && c == IRDouble && e = IRFpext a b c
        | a == IRFloat && c == IRDouble && not e = IRFptrunc c d a
        | a == IRFloat && c == IRLongDouble && e = IRFpext a b c
        | a == IRFloat && c == IRLongDouble && not e = IRFptrunc c d a
        | a == IRDouble && c == IRFloat && e = IRFpext c d a
        | a == IRDouble && c == IRFloat && not e = IRFptrunc a b c
        | a == IRDouble && c == IRLongDouble && e = IRFpext a b c
        | a == IRDouble && c == IRLongDouble && not e = IRFptrunc c d a
        | a == IRLongDouble && c == IRFloat && e = IRFpext c d a
        | a == IRLongDouble && c == IRFloat && not e = IRFptrunc a b c
        | a == IRLongDouble && c == IRDouble && e = IRFpext c d a
        | a == IRLongDouble && c == IRDouble && not e = IRFptrunc a b c
        | isIntegral a && isFloating c && e = IRSitofp a b c
        | isFloating a && isIntegral c && e = IRSitofp c d a
        | isIntegral a && isFloating c && not e = IRFptosi c d a
        | isFloating a && isIntegral c && not e = IRSitofp c d a
        | otherwise =  error ""

      {-
        binaryInstruction receives a binary operation symbol as a string (a), and the types of the two arguments for the
        operation (a, and b) to ensure that they are the same type.
      -}
      binaryInstruction a b
        | a == "*" && isIntegral b = IRMul
        | a == "*" && isFloating b = IRFmul
        | a == "/" && isFloating b = IRFdiv
        | a == "/" && isIRUnsigned b = IRUdiv
        | a == "/" && isIRSigned b = IRSdiv
        | a == "%" && isIRUnsigned b = IRUrem
        | a == "%" && isIRSigned b = IRSrem
        | a == "%" && isFloating b = IRFrem
        | a == "+" && isIntegral b = IRAdd
        | a == "+" && isFloating b = IRFadd
        | a == "-" && isIntegral b = IRSub
        | a == "-" && isFloating b = IRFsub
        | a == "<<" && isIntegral b = IRShl
        | a == ">>" && isIntegral b = IRAshr
        | a == "<" && isIRUnsigned b = IRIcmp IRIUlt
        | a == "<" && isIRSigned b = IRIcmp IRISlt
        | a == "<" && isFloating b = IRFcmp IRFOlt
        | a == ">" && isIRUnsigned b = IRIcmp IRIUgt
        | a == ">" && isIRSigned b = IRIcmp IRISgt
        | a == ">" && isFloating b = IRFcmp IRFOgt
        | a == "<=" && isIRUnsigned b = IRIcmp IRIUle
        | a == "<=" && isIRSigned b = IRIcmp IRISle
        | a == "<=" && isFloating b = IRFcmp IRFOle
        | a == ">=" && isIRUnsigned b = IRIcmp IRIUge
        | a == ">=" && isIRSigned b = IRIcmp IRISge
        | a == ">=" && isFloating b = IRFcmp IRFOge
        | a == "==" && isIntegral b = IRIcmp IRIEq
        | a == "==" && isFloating b = IRFcmp IRFOeq
        | a == "!=" && isIntegral b = IRIcmp IRINe
        | a == "!=" && isFloating b = IRFcmp IRFOne
        | a == "&" && isIntegral b = IRAnd
        | a == "^" && isIntegral b = IRXor
        | a == "|" && isIntegral b = IROr
        | otherwise = error ""

      compound (CCompound a b) = CCompound a b
      compound a = CCompound Nothing (Just (CList [a]))

      {-
        appendBlocks receives two lists of basic blocks (a, and b) to appended together. the first block of b is appended to
        the last block of a and the remaining blocks of b are appended to the end which results in the modification of the last
        block of a. When modification is not desired and a new block should be started, append an empty block to a beforehand.
      -}
      appendBlocks a b = init a ++ [last a ++ head b] ++ tail b

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

  generateIRFunctionGlobal (CFunction (Just a) b _ c) = [IRFunctionGlobal functionType (name b) (map argument (argumentList b)) (evalState (generateIRBasicBlocks c functionType) (GeneratorState [[]] 1 Map.empty Nothing))]
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

  generate = generateIRModule
