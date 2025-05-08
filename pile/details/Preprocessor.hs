module Preprocessor where
  preprocess f = filter (/='\n') f
