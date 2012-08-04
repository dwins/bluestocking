module Main where

import Wikipedia hiding (main)
import Doxament
import Parse

text :: String
text = "Uruguay is not in South America"

terms :: [Term]
terms = removeStops $ concat $ sentences text

justs :: [Maybe a] -> [a]
justs (Just a : rest)  = a : justs rest
justs (Nothing : rest) = justs rest
justs [] = []

buildCorpus :: [Maybe String] -> Doxament
buildCorpus articles = foldl (\corpus a -> merge corpus $ relations a) [] $ justs articles 

main :: IO ()
main = do articles <- mapM (fetchArticle "simple") terms
          putStrLn "Building knowledge base"
          let corpus = buildCorpus articles
          putStrLn "Querying against original document"
          let res = query corpus $ relations text
          print res
