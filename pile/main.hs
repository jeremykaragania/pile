module Main where
  import CodeEmitter
  import IRGenerator
  import Lexer
  import Parser
  import Scheduler
  import Selector
  import System.Environment

  parseInFile [a] = a

  parseOutFile a = take (length a - 2) a ++ ".s"

  isInFile a = drop (length a - 2) a == ".c"

  main = do
    args <- getArgs
    let inFile = parseInFile args
    if isInFile inFile then do
      file <- readFile inFile
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
              writeFile (parseOutFile inFile) assembly
              return ()
    else error ""
