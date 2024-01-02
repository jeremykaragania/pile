module Main where
  import CodeEmitter
  import IRGenerator
  import Lexer
  import Parser
  import Scheduler
  import Selector
  import System.Environment

  data Options = Options [String] (Maybe String) deriving Show

  inFiles (Options a _) = a

  outFile (Options _ (Just a)) = a

  outFile (Options _ Nothing) = "a.out"

  parseArgs [] args = args

  parseArgs (a:as) (Options inFiles outFile)
    | a == "-o" = parseArgs (drop 1 as) (Options inFiles (Just (head as)))
    | otherwise = parseArgs as (Options (inFiles ++ [a]) outFile)

  sourceCode a = drop (length a - 2) a == ".c"

  main = do
    args <- getArgs
    let opt = parseArgs args (Options [] Nothing)
    let isSourceCode = all sourceCode (inFiles opt)
    if isSourceCode then do
      file <- readFile ((head . inFiles) opt)
      let tokens = scan file
      case tokens of
        Left x -> print x
        Right x -> do
          let tree = parse x
          case tree of
            Left x -> print x
            Right x -> do
              let ir = generateIR x
              let graph = select ir
              let machineCodes = schedule graph
              let assembly = emit machineCodes
              return ()
      else
        error ""
