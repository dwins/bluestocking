module Doxament where

import Parse

type Doxament = [Relation]
type Score = Double
type Contradictions = [Relation]

score :: (Score, a) -> Score
score = fst

corpusAgree :: Doxament -> Relation -> Bool
corpusAgree d r = any (agree r) d

query :: Doxament -> Doxament -> (Score, Contradictions)
query corpus q = (score', contradictions)
    where contradictions = (filter (corpusAgree corpus) . map flipRelation) q
          agreements = filter (corpusAgree corpus) q 
          score' = count / total 
          count = fromIntegral (length agreements - length contradictions)
          total = fromIntegral (length corpus)

merge :: Doxament -> Doxament -> Doxament
merge = (++)
