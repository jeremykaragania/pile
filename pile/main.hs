module Main where
  import Generator
  import Lexer
  import Parser
  import System.Environment

  data Options = Options [String] (Maybe String) deriving Show

  inFiles (Options a _) = a

  outFile (Options _ (Just a)) = a

  outFile (Options _ Nothing) = "a.out"

  parseArgs [] args = args

  parseArgs (a:as) (Options inFiles outFile)
    | a == "-o" = parseArgs (drop 1 as) (Options inFiles (Just (head as)))
    | otherwise = parseArgs as (Options (a : inFiles) outFile)

  main = do
    args <- getArgs
    let opt = parseArgs args (Options [] Nothing)
    file <- readFile ((head . inFiles) opt)
    let tokens = scan file
    case tokens of
      Left x -> print x
      Right x -> do
        let tree = parse x
        case tree of
          Left x -> print x
          Right x -> do
            writeFile (outFile opt) (generate x)
    return ()
