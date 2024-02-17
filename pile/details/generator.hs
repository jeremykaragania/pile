module Generator where
  import Control.Monad.State
  import Data.Char (toLower, ord)
  import Data.List (intercalate)
  import Data.Map (Map)
  import qualified Data.Map as Map
  import Data.Set (fromList)
  import Syntax

  {-
    GeneratorState carries state between generators. A GeneratorState carries past basic blocks ("blocks"), an accumulator
    ("counter") for unnamed temporaries, a lookup table ("table") which associates an identifier key with an IdentInfo, and a
    context for break and continue statements ("context") which carries the integer label number for the branch.
  -}
  data GeneratorState = GeneratorState {
    blocks :: [[(Maybe IRLabel, IRInstruction)]],
    counter :: Integer,
    table :: Map String [IdentInfo],
    context :: (Maybe Integer),
    globals :: [IRGlobalValue],
    scope :: Integer}

  {-
    IdentInfo carries identifier information which provides context when performing a lookup. Since identifiers can have the
    same name, this is just a way to differentiate between them when scopes are nested.
  -}
  data IdentInfo = IdentInfo {
    identScope :: Integer,
    identLabel :: IRLabel,
    identType :: IRType} deriving (Show)

  {-
    InstrInfo carries instruction information which is useful for querying past generated instructions. Most instructions
    (the items of the list in a basic block) contain a type and a destination in their actual syntax. When we speak of a
    destination, we mean the label which the instruction stores its result to.
  -}
  data InstrInfo = InstrInfo {
    instrType :: IRType,
    instrTo :: IRLabel}

  setBlocks a (GeneratorState _ b c d e f) = GeneratorState a b c d e f

  setCounter a (GeneratorState b c d e f g) = GeneratorState b (a c) d e f g

  setTable a (GeneratorState b c _ d e f) = GeneratorState b c a d e f

  setContext a (GeneratorState b c d _ e f) = GeneratorState b c d a e f

  setGlobals a (GeneratorState b c d e _ f) = GeneratorState b c d e a f

  setLevel a (GeneratorState b c d e f g) = GeneratorState b c d e f (a g)

  type GeneratorStateMonad = State GeneratorState

  declarationList (Just (CDeclarationList a)) = a
  declarationList Nothing = []

  statementList (Just (CList a)) = a
  statementList Nothing = []

  compound (CCompound a b) = CCompound a b
  compound a = CCompound Nothing (Just (CList [a]))

  getIdentifier (CInitDeclarator a _) = getIdentifier a
  getIdentifier (CDirectDeclaratorIdentifier a) = (identifier . token) a
  getIdentifier (CDeclarator _ a) = getIdentifier a

  getPointer (CInitDeclarator a _) = getPointer a
  getPointer (CDeclarator a _) = a

  getConstant (CInitDeclarator _ a) = a
  getConstant (CDeclarator _ _) = CConstant (CConstantToken (CIntegerConstant 0 Nothing))

  getInstrInfo (Just a, (IRAdd b _ _)) = InstrInfo b a
  getInstrInfo (Just a, (IRFadd b _ _)) = InstrInfo b a
  getInstrInfo (Just a, (IRSub b _ _)) = InstrInfo b a
  getInstrInfo (Just a, (IRFsub b _ _)) = InstrInfo b a
  getInstrInfo (Just a, (IRMul b _ _)) = InstrInfo b a
  getInstrInfo (Just a, (IRFmul b _ _)) = InstrInfo b a
  getInstrInfo (Just a, (IRUdiv b _ _)) = InstrInfo b a
  getInstrInfo (Just a, (IRSdiv b _ _)) = InstrInfo b a
  getInstrInfo (Just a, (IRFdiv b _ _)) = InstrInfo b a
  getInstrInfo (Just a, (IRUrem b _ _)) = InstrInfo b a
  getInstrInfo (Just a, (IRSrem b _ _)) = InstrInfo b a
  getInstrInfo (Just a, (IRFrem b _ _)) = InstrInfo b a
  getInstrInfo (Just a, (IRShl b _ _)) = InstrInfo b a
  getInstrInfo (Just a, (IRLshr b _ _)) = InstrInfo b a
  getInstrInfo (Just a, (IRAshr b _ _)) = InstrInfo b a
  getInstrInfo (Just a, (IRAnd b _ _)) = InstrInfo b a
  getInstrInfo (Just a, (IROr b _ _)) = InstrInfo b a
  getInstrInfo (Just a, (IRXor b _ _)) = InstrInfo b a
  getInstrInfo (Just a, (IRAlloca b)) = InstrInfo b a
  getInstrInfo (Just a, (IRLoad b _)) = InstrInfo b a
  getInstrInfo (_, (IRStore a _ b)) = InstrInfo a b
  getInstrInfo (Just a, (IRIcmp _ _ _ _)) = InstrInfo (IRInteger IRSigned) a
  getInstrInfo (Just a, (IRFcmp _ _ _ _)) = InstrInfo (IRInteger IRSigned) a
  getInstrInfo (Just a, (IRSext _ _ b)) = InstrInfo b a
  getInstrInfo (Just a, (IRFptrunc _ _ b)) = InstrInfo b a
  getInstrInfo (Just a, (IRFpext _ _ b)) = InstrInfo b a
  getInstrInfo (Just a, (IRFptoui _ _ b)) = InstrInfo b a
  getInstrInfo (Just a, (IRFptosi _ _ b)) = InstrInfo b a
  getInstrInfo (Just a, (IRUitofp _ _ b)) = InstrInfo b a
  getInstrInfo (Just a, (IRSitofp _ _ b)) = InstrInfo b a
  getInstrInfo (Just a, (IRPtrtoint _ _ b)) = InstrInfo b a
  getInstrInfo (Just a, (IRInttoptr _ _ b)) = InstrInfo b a
  getInstrInfo (Just a, (IRBitcast _ _ b)) = InstrInfo b a
  getInstrInfo (Just a, (IRAddrspacecast _ _ b)) = InstrInfo b a

  getInstrType = (instrType . getInstrInfo)

  getInstrTo = (instrTo . getInstrInfo)

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
    | areSpecifiers ["void"] = IRVoid
    | areSpecifiers ["short"] = IRShortInteger IRSigned
    | areSpecifiers ["signed", "short"] = IRShortInteger IRSigned
    | areSpecifiers ["short", "int"] = IRShortInteger IRSigned
    | areSpecifiers ["signed", "short", "int"] = IRShortInteger IRSigned
    | areSpecifiers ["unsigned", "short"] = IRShortInteger IRUnsigned
    | areSpecifiers ["unsigned", "short", "int"] = IRShortInteger IRUnsigned
    | areSpecifiers ["int"] = IRInteger IRSigned
    | areSpecifiers ["signed"] = IRInteger IRSigned
    | areSpecifiers ["signed", "int"] = IRInteger IRSigned
    | areSpecifiers ["unsigned"] = IRInteger IRUnsigned
    | areSpecifiers ["unsigned", "int"] = IRInteger IRUnsigned
    | areSpecifiers ["long"] = IRLongInteger IRSigned
    | areSpecifiers ["signed", "long"] = IRLongInteger IRSigned
    | areSpecifiers ["long", "int"] = IRLongInteger IRSigned
    | areSpecifiers ["signed", "long", "int"] = IRLongInteger IRSigned
    | areSpecifiers ["unsigned", "long"] = IRLongInteger IRUnsigned
    | areSpecifiers ["unsigned", "long", "int"] = IRLongInteger IRUnsigned
    | areSpecifiers ["char"] = IRInteger IRSigned
    | areSpecifiers ["signed", "char"] = IRInteger IRSigned
    | areSpecifiers ["unsigned", "char"] = IRInteger IRUnsigned
    | areSpecifiers ["float"] = IRFloat
    | areSpecifiers ["double"] = IRDouble
    | areSpecifiers ["long", "double"] = IRLongDouble
    where
      areSpecifiers b
        | length a == (length . fromList) a = fromList a == fromList (map (\c -> (CTypeSpecifier (CKeywordToken c))) b)
        | otherwise = error ""

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
    appendBlocks receives two lists of basic blocks (a, and b) to appended together. the first block of b is appended to
    the last block of a and the remaining blocks of b are appended to the end which results in the modification of the last
    block of a. When modification is not desired and a new block should be started, append an empty block to a beforehand.
  -}
  appendBlocks a b = init a ++ [last a ++ head b] ++ tail b

  {-
    getCastInstr receives two type-value pairs (a, and b), with different types and cast specifier (c). It will perform
    a cast operation to either a or b according to c such that a, and b both have the same type. If c is False, the cast is
    a downcast, and if c is True, the cast is an upcast.
  -}
  getCastInstr (a, b) (c, d) e
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
    | otherwise = error ""

  {-
    getBinaryInstr receives a binary operation symbol as a string (a), and the types of the two arguments for the
    operation (a, and b) to ensure that they are the same type.
  -}
  getBinaryInstr a b
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

  newAlloca :: IRType -> GeneratorStateMonad ()
  newAlloca a = do
    got <- get
    let alloca = [[(Just (IRLabelNumber (counter got)), IRAlloca a)]]
    let newBlocks = appendBlocks (blocks got) alloca
    put ((setBlocks newBlocks . setCounter (+1)) got)

  newCast :: (IRType, IRValue) -> (IRType, IRValue) -> Bool -> GeneratorStateMonad ()
  newCast a b c
    | isSameType (fst a) (fst b) == Nothing = do
      got <- get
      let instrs = [[(Just (IRLabelNumber (counter got)), getCastInstr a b c)]]
      let newBlocks = appendBlocks (blocks got) instrs
      put ((setBlocks newBlocks . setCounter (+1)) got)
    | otherwise = return ()

  newLabeled :: CStatement -> GeneratorStateMonad ()
  newLabeled a = do
    got <- get
    let stat = execState ((generateCStatement . compound) a) got
    let switchBr = [(Nothing, IRBrUnconditional (IRLabelValue (IRLabelNumber ((counter stat)))))]
    let newBlocks = appendBlocks (blocks stat) [switchBr, []]
    put ((setBlocks newBlocks . setCounter (+1)) stat)

  {-
    All selection statements consist of an expression (a), and at least one body (b) which is a compound statement.
    Therefore, newSelectionHead generates this selection statement head so it can be used in other selection statement
    generators.
  -}
  newSelectionHead :: CExpression -> CStatement -> GeneratorStateMonad ()
  newSelectionHead (CExpression a) b = do
    got <- get
    let expr = execState (generateCExpressions a) got
    let exprType = (getInstrType . last . concat . blocks) expr
    let cmp = (Just (IRLabelNumber (counter expr)), comparisonInstruction (exprType, counter expr))
    let stat = execState (generateCStatement b) (setCounter (+1) expr)
    let br0 = (Nothing, IRBrConditional (getInstrType cmp) (IRLabelValue (IRLabelNumber (counter expr))) (IRLabelValue (IRLabelNumber (counter expr + 1))) (IRLabelValue (IRLabelNumber (counter stat + 1))))
    let newBlocks = appendBlocks (blocks stat) (appendBlocks [[cmp]] [[br0]])
    put ((setBlocks newBlocks . setCounter (+2)) expr)
    where
      -- Different comparison instructions are used depending on argument type.
      comparisonInstruction a
        | (isIntegral . fst) a = IRIcmp IRINe (fst a) (IRLabelValue (IRLabelNumber (snd a - 1))) (IRConstantValue (generateIRConstant (CConstant (CConstantToken (CIntegerConstant 0 Nothing))) (fst a)))
        | (isFloating . fst) a = IRFcmp IRFOne (fst a) (IRLabelValue (IRLabelNumber (snd a - 1))) (IRConstantValue (generateIRConstant (CConstant (CConstantToken (CIntegerConstant 0 Nothing))) (fst a)))

  {-
    newFunctionBody is used for the generation of basic blocks, which are just instruction lists, from a function body,
    which is just a compound statement. It receives a compound statement (a), and the return type of the function (b).
  -}
  newFunctionBody :: CStatement -> IRType -> GeneratorStateMonad [IRBasicBlock]
  newFunctionBody a b = do
    got <- get
    let stat = execState (generateCStatement a) got
    return (fst (execState ((numberBlocks . blocks) stat) ([], 0)))
    where
      {-
        It is hard to number blocks as they are being generated because there is not enough context. Therefore, block
        numbering is the last step. A block number can be calculated by counting the past numbered instructions in past blocks.
        The state tracks the list of blocks which have been numbered and the count of past numbered instructions.
      -}
      numberBlocks :: [[(Maybe IRLabel, IRInstruction)]] -> State ([IRBasicBlock], Integer) ()
      numberBlocks [] = return ()
      numberBlocks (a:as) = do
        got <- get
        let present = (toInteger . length . filter numbered) a
        let past = snd got
        put (((fst got) ++ [IRBasicBlock (IRLabelNumber past) a], present + past + 1))
        numberBlocks as
        where
          numbered (Nothing, _) = False
          numbered _ = True

  {-
    The last instruction of a generated expression will contain the resulting value of that expression. This makes it
    easier to get the resulting type of an expression after generation.
  -}
  generateCExpressions :: [CExpression] -> GeneratorStateMonad ()
  generateCExpressions [] = return ()

  generateCExpressions (a:as) = do
    generateCExpression a
    generateCExpressions as

  generateCExpression :: CExpression -> GeneratorStateMonad ()
  generateCExpression (CIdentifier a) = do
    got <- get
    let ident = (last . filter (\b -> (identScope b) <= (scope got))) ((table got) Map.! (identifier a)) -- We use the identifier closest to the current scope.
    let instrs = [[((Just (IRLabelNumber (counter got))), IRLoad (identType ident) (IRLabelValue (identLabel ident)))]]
    let newBlocks = appendBlocks (blocks got) instrs
    put ((setBlocks newBlocks . setCounter (+1)) got)

  generateCExpression a@(CConstant b) = do
    got <- get
    let instrs = [[(Just (IRLabelNumber (counter got)), IRAlloca ((typeFromCConstant . constant) b)), (Nothing, IRStore ((typeFromCConstant . constant) b) (IRConstantValue (generateIRConstant a ((typeFromCConstant . constant) b))) (IRLabelNumber (counter got)))]]
    let newBlocks = appendBlocks (blocks got) instrs
    put ((setBlocks newBlocks . setCounter (+1)) got)

  generateCExpression (CBinary a b c) = do
    got <- get
    let expr0 = execState (generateCExpression b) got
    let type0 = (getInstrType . last . concat . blocks) expr0
    if a == "=" then do
      let expr1 = execState (generateCExpression c) got
      let type1 = (getInstrType . last . concat . blocks) expr1
      let castExpr = execState (newCast (type0, IRLabelValue (IRLabelNumber (counter expr1 - 1))) (type1, IRLabelValue (IRLabelNumber (counter expr1 - 1))) False) expr1
      put castExpr
    else do
      let expr1 = execState (generateCExpression c) expr0
      let type1 = (getInstrType . last . concat . blocks) expr1
      let castExpr = execState (newCast (type0, IRLabelValue (IRLabelNumber (counter expr0 - 1))) (type1, IRLabelValue (IRLabelNumber (counter expr1 - 1))) True) expr1
      let castType = (getInstrType . last . concat) (blocks castExpr)
      let bin = binary (type0, counter expr0) (type1, counter expr1) (castType, counter castExpr)
      let newBlocks = appendBlocks (blocks castExpr) [[(Just (IRLabelNumber (counter castExpr)), bin)]]
      put ((setBlocks newBlocks . setCounter (+1)) castExpr)
    where
      {-
        Selects the expression which was not casted and uses it to generate the binary instruction. Ensures that the binary
        instruction is not generated with duplicate arguments.
      -}
      binary d e f
        | fst d == fst f = (getBinaryInstr a (fst f)) (fst f) (IRLabelValue (IRLabelNumber (snd d - 1))) (IRLabelValue (IRLabelNumber (snd f - 1)))
        | fst e == fst f = (getBinaryInstr a (fst f)) (fst f) (IRLabelValue (IRLabelNumber (snd f - 1))) (IRLabelValue (IRLabelNumber (snd e - 1)))

  generateCExpression (CAssignment a b@(CIdentifier c) d) = do
    got <- get
    let ident = (last . filter (\e -> (identScope e) <= (scope got))) ((table got) Map.! (identifier c))
    let expr = execState (generateCExpression (CBinary (getOperator a) b d)) got
    let newBlocks = appendBlocks (blocks expr) [[(Nothing, IRStore (identType ident) (IRLabelValue (IRLabelNumber (counter expr - 1))) (identLabel ident))]]
    put (setBlocks newBlocks expr)

  generateCExpression (CExpression []) = return ()

  generateCExpression (CExpression (a:as)) = do
    got <- get
    let expr = execState (generateCExpression a) got
    put expr
    generateCExpression (CExpression as)
  generateCDeclarations :: [CDeclaration] -> GeneratorStateMonad ()
  generateCDeclarations [] = return ()

  generateCDeclarations (a:as) = do
    generateCDeclaration a
    generateCDeclarations as

  generateCDeclaration :: CDeclaration -> GeneratorStateMonad ()
  generateCDeclaration (CDeclaration d (Just (CInitDeclaratorList []))) = return ()

  generateCDeclaration (CDeclaration a (Just (CInitDeclaratorList (b:bs)))) = do
    got <- get
    let type0 = typeFromCSpecifiers a (getPointer b)
    let alloca = execState (newAlloca type0) got
    let dec = execState (generateCDeclaration b) alloca
    let type1 = (getInstrType . last . concat . blocks) dec
    let castExpr = execState (newCast (type0, IRLabelValue (IRLabelNumber (counter got))) (type1, IRLabelValue (IRLabelNumber (counter dec - 1))) False) dec
    let store = [[(Nothing, IRStore (typeFromCSpecifiers a (getPointer b)) (IRLabelValue (IRLabelNumber (counter castExpr - 1))) (IRLabelNumber (counter got)))]]
    let newBlocks = appendBlocks (blocks castExpr) store
    case Map.lookup (getIdentifier b) (table got) of
      Just c -> do
        if all (\d -> identScope d /= (scope got)) c then do
          let newTable = Map.insert (getIdentifier b) (c ++ [(IdentInfo (scope got) (IRLabelNumber (counter got)) type0)]) (table got)
          put ((setBlocks newBlocks . setTable newTable) castExpr)
        else error ""
      Nothing -> do
        let newTable = Map.insert (getIdentifier b) [(IdentInfo (scope got) (IRLabelNumber (counter got)) type0)] (table got)
        put ((setBlocks newBlocks . setTable newTable) castExpr)
    generateCDeclaration (CDeclaration a (Just (CInitDeclaratorList bs)))

  generateCDeclaration (CInitDeclarator _ b) = generateCExpression b

  generateCDeclaration (CDeclarator _ b) = return ()

  generateCStatements :: [CStatement] -> GeneratorStateMonad ()
  generateCStatements [] = return ()

  generateCStatements (a:as) = do
    got <- get
    generateCStatement a
    generateCStatements as

  generateCStatement :: CStatement -> GeneratorStateMonad ()
  generateCStatement (CLabeledCase a b) = newLabeled b

  generateCStatement (CLabeledDefault a) = newLabeled a

  generateCStatement (CList a) = generateCStatements a

  generateCStatement (CCompound a b) = do
    got <- get
    let dec = execState ((generateCDeclarations . declarationList) a) (setLevel (+1) got)
    let stat = execState ((generateCStatements . statementList) b) dec
    put (setLevel (+ (-1)) stat)

  generateCStatement (CCExpression (Just (CExpression []))) = return ()

  generateCStatement (CCExpression (Just (CExpression a))) = generateCExpressions a

  generateCStatement (CIf a@(CExpression _) b) = do
    got <- get
    let ifHead = execState (newSelectionHead a (compound b)) got
    let ifBody = execState ((generateCStatement . compound) b) (setBlocks ((blocks ifHead) ++ [[]]) ifHead)
    let ifBr = [(Nothing, IRBrUnconditional (IRLabelValue (IRLabelNumber ((counter ifBody)))))]
    let newBlocks = appendBlocks (blocks ifBody) [ifBr, []]
    put ((setBlocks newBlocks . setCounter (+1)) ifBody)

  generateCStatement (CIfElse a@(CExpression _) b c) = do
    got <- get
    let ifHead = execState (newSelectionHead a (compound b)) got
    let ifBody = execState ((generateCStatement . compound) b) (setBlocks ((blocks ifHead) ++ [[]]) ifHead)
    let elseBody = execState ((generateCStatement . compound) c) ((setBlocks ((blocks ifBody) ++ [[]]) . setCounter (+1)) ifBody)
    let elseBr = [[(Nothing, IRBrUnconditional (IRLabelValue (IRLabelNumber ((counter elseBody)))))]]
    let newBlocks = appendBlocks (blocks elseBody) elseBr
    put ((setBlocks newBlocks . setCounter (+1)) elseBody)

  generateCStatement (CSwitch (CExpression a) b) = do
    got <- get
    let splitStat = (CCompound Nothing . Just . CList . splitLabeled . statementList . statementFromCCompound . compound) b
    let expr = execState (generateCExpressions a) got
    let exprType = (getInstrType . last . concat . blocks) expr
    if (isIntegral exprType) then do
      let stat = execState ((switchStatement . splitLabeled . statementList . statementFromCCompound . compound) b) ((GeneratorState [[], []] ((counter expr) + 1) (table expr) (Just ((counter . fst) stat)) (globals got) (scope got)), ([(counter . fst) stat], []))
      let switch = [(Nothing, IRSwitch exprType (IRLabelNumber ((fromIntegral . length) ((snd . snd) stat))) (IRLabelNumber (labeledDefault ((fst . snd) stat))) ((snd . snd) stat))]
      let switchBr = [(Nothing, IRBrUnconditional (IRLabelValue (IRLabelNumber (((counter . fst) stat)))))]
      let newBlocks = appendBlocks (blocks expr) (appendBlocks [switch] (appendBlocks ((blocks . fst) stat) [switchBr, []]))
      put ((setBlocks newBlocks . setCounter (+1) . setContext Nothing . fst) stat)
    else error ""
    where
      splitLabeled [] = []
      splitLabeled (a:as)
        | isLabeled a = a:as
        | otherwise = splitLabeled as
      isLabeled (CLabeledCase _ _) = True
      isLabeled (CLabeledDefault _) = True
      isLabeled _ = False
      -- Decides whether to use the explicit (a) or implicit (b) default label.
      labeledDefault [a] = a
      labeledDefault [a, b] = b
      labeledDefault _ = error ""

  generateCStatement (CReturn Nothing) = do
    got <- get
    let instrs = [[(Nothing, IRRet Nothing)]]
    let newBlocks = appendBlocks (blocks got) instrs
    put (setBlocks newBlocks got)

  generateCStatement (CReturn (Just a)) = do
    got <- get
    let expr = execState (generateCExpression a) got
    let exprLabel = (getInstrTo . last . concat . blocks) expr
    let instrs = [[(Nothing, IRRet (Just (IRLabelValue exprLabel)))]]
    let newBlocks = appendBlocks (blocks expr) instrs
    put (setBlocks newBlocks expr)

  generateCStatement (CBreak) = do
    got <- get
    case (context got) of
      Just a -> do
        let instrs = [[(Nothing, IRBrUnconditional (IRLabelValue (IRLabelNumber (a))))]]
        let newBlocks = appendBlocks (blocks got) instrs
        put (setBlocks newBlocks got)
      otherwise -> error ""

  switchStatement :: [CStatement] -> State (GeneratorState, ([Integer], [(IRType, IRConstant, IRLabel)])) ()
  switchStatement [] = return ()

  switchStatement (a:as) = do
    got <- get
    let stat = execState (generateCStatement a) (fst got)
    let newBlocks = blocks stat
    case a of
      CLabeledCase a _ -> do
        let switchConstant = (constant . token) a
        let switchType = typeFromCConstant switchConstant
        put ((stat, ((fst . snd) got, (snd . snd) got ++ [(switchType, generateIRConstant a switchType, IRLabelNumber ((counter . fst) got - 1))])))
        switchStatement as
      CLabeledDefault _ -> do
        put ((stat, ((fst . snd) got ++ [(counter . fst) got - 1], (snd . snd) got)))
        switchStatement as
      otherwise -> do
        put ((stat, ((fst . snd) got, (snd . snd) got)))
        switchStatement as

  generateCExternalDefinition :: CExternalDefinition -> GeneratorStateMonad ()
  generateCExternalDefinition (CFunction (Just a) b c d) = do
    got <- get
    let basicBlocks = evalState (newFunctionBody d functionType) (GeneratorState [[]] 1 (table got) Nothing [] 0)
    let functionGlobal = IRFunctionGlobal functionType (name b) [] basicBlocks
    let newGlobals = (globals got) ++ [functionGlobal]
    put ((setGlobals newGlobals) got)
    where
      functionType = typeFromCSpecifiers a (pointer b)
      name (CDeclarator _ (CDirectDeclaratorIdentifier (CIdentifier (CIdentifierToken a)))) = a
      name (CDeclarator _ (CDirectDeclaratorFunctionCall (CDirectDeclaratorIdentifier (CIdentifier (CIdentifierToken a))) _)) = a
      pointer (CDeclarator a _) = a
      argumentList (CDeclarator _ (CDirectDeclaratorFunctionCall _ [CParameterList a])) = a
      argument (CParameterDeclaration c d) = IRArgument (typeFromCSpecifiers c Nothing) (Just (name d))

  generateCExternalDefinition (CExternalDeclaration (CDeclaration a (Just (CInitDeclaratorList b)))) = do
    got <- get
    let dec = execState (declarations b) got
    put dec
    where
      declarations :: [CDeclaration] -> GeneratorStateMonad ()
      declarations [] = return ()

      declarations (b:bs) = do
        got <- get
        let name = getIdentifier b
        if Map.notMember name (table got) then do
          let varType = typeFromCSpecifiers a (getPointer b)
          let var = IRVariableGlobal name varType (generateIRConstant (getConstant b) varType)
          let newTable = Map.insert name [(IdentInfo (scope got) (IRLabelName name) varType)] (table got)
          let newGlobals = globals got ++ [var]
          put ((setTable newTable . setGlobals newGlobals) got)
          declarations bs
        else error ""

  generateCExternalDefinitions :: [CExternalDefinition] -> GeneratorStateMonad ()
  generateCExternalDefinitions [] = return ()

  generateCExternalDefinitions (a:as) = do
    got <- get
    let def = execState (generateCExternalDefinition a) got
    put def
    generateCExternalDefinitions as

  generateCTranslationUnit :: CTranslationUnit -> GeneratorStateMonad IRModule
  generateCTranslationUnit (CTranslationUnit a) = do
    got <- get
    let globalValues = (globals . execState (generateCExternalDefinitions a)) got
    return (IRModule globalValues)

  generate a = evalState (generateCTranslationUnit a) (GeneratorState [[]] 1 Map.empty Nothing [] 0)
