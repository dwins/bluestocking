module Main where

import System.Environment
import Wikipedia hiding (main)
import Doxament
import Parse

justs :: [Maybe a] -> [a]
justs (Just a : rest)  = a : justs rest
justs (Nothing : rest) = justs rest
justs [] = []

buildCorpus :: [Maybe String] -> Doxament
buildCorpus articles = foldl add [] articles'
    where add corpus a = merge corpus $ relations a
          articles' = justs articles

noisyFetchArticle :: String -> IO (Maybe String)
noisyFetchArticle topic = do putStrLn $ "Looking up " ++ topic
                             fetchArticle "simple" topic

queryFromArgs :: String -> [String] -> String
queryFromArgs def args | null args = def
                       | otherwise = head args

main :: IO ()
main = do args <- getArgs
          let q = queryFromArgs "Uruguay is not in South America." args
          let terms = (removeStops . concat . sentences) q
          articles <- mapM noisyFetchArticle terms
          putStrLn "Building knowledge base"
          let corpus = buildCorpus articles
          putStrLn "Querying against original document"
          let res = query corpus $ relations q
          print res
