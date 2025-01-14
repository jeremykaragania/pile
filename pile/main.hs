module Main where
  import Allocator
  import Emitter
  import Generator
  import Lexer
  import Optimizer
  import Parser
  import Scheduler
  import Selector
  import System.FilePath
  import System.Environment

  usage = "filename..."

  parseOutFile a = takeFileName (take (length a - 2) a ++ ".s")

  isInFile a = drop (length a - 2) a == ".c"

  compile [] = return ()

  compile (a:as) = do
    if isInFile a then do
      file <- readFile a
      let scanned = scan file
      case scanned of
        Left b -> print b
        Right b -> do
          let parsed = parse b
          case parsed of
            Left _ -> print b
            Right c -> do
              let generated = generate c
              let selected = select generated
              let scheduled = schedule selected
              let allocated = allocate scheduled
              let optimized = optimize allocated
              let emitted = emit optimized
              writeFile (parseOutFile a) emitted
    else error ""
    compile as

  main = do
    args <- getArgs
    prog <- getProgName
    case args of
      [] -> putStrLn ("Usage: " ++ prog ++ " " ++ usage)
      _ -> compile args
