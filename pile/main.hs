module Main where
  import Allocator
  import Emitter
  import Generator
  import Lexer
  import Optimizer
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
      let scanned = scan file
      case scanned of
        Left a -> print a
        Right a -> do
          let parsed = parse a
          case parsed of
            Left a -> print a
            Right a -> do
              let generated = generate a
              let selected = select generated
              let scheduled = schedule selected
              let allocated = allocate scheduled
              let optimized = optimize allocated
              let emitted = emit optimized
              writeFile (parseOutFile inFile) emitted
              return ()
    else error ""
