module Main where
  import System.Environment

  data Argument =
    InFile String |
    OutFile String deriving Show

  parseArgs :: [String] -> [Argument] -> [Argument]

  parseArgs [] args = args

  parseArgs [x] args = do
    parseArgs [] (InFile x : args)

  parseArgs (x:xs) args = do
    case x of
      "-o" -> do
        parseArgs (drop 1 xs) (OutFile (head xs) : args)
      otherwise -> do
        parseArgs xs (InFile x : args)

  main = do
    files <- getArgs
    return ()
