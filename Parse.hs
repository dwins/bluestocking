module Parse where

data Relation = Relation {
    co :: Bool,
    subject :: Term,
    object ::  Term
  } deriving (Show, Eq)

type Document = String
type Term = String

-- TODO: Get real stop word list
stopWords :: [Term] 
stopWords = ["a", "was", "is"]

agree :: Relation -> Relation -> Bool
agree (Relation c s o) (Relation c' s' o') =
    (c == c' && s == s' && o == o') || 
    (c == c' && s == o' && o == s')

isNeg :: (Bool, Term) -> Bool
isNeg = fst

stripNeg :: (Bool, Term) -> Term
stripNeg = snd

flipRelation :: Relation -> Relation
flipRelation (Relation c l r) = Relation (not c) r l

removeStops :: [Term] -> [Term]
removeStops = filter (`notElem` stopWords)

-- TODO: Serious implementation
sentences :: Document -> [[Term]]
sentences =  map (removeStops . words) . sentenceStrings
  where sentenceStrings :: String -> [String]
        sentenceStrings [] = []
        sentenceStrings cs = sent : (sentenceStrings . drop 1) rest
            where (sent, rest) = break (== '.') cs

pairs :: [a] -> [(a, a)]
pairs (x : xs) = map (\y -> (x, y)) xs ++ pairs xs
pairs []       = []

relations :: Document -> [Relation]
relations doc = concatMap sentenceRelations $ sentences doc
  where sentenceRelations sent = map mkRelation ((pairs . negScope) sent)

mkRelation :: ((Bool, Term), (Bool, Term)) -> Relation
mkRelation ((c, w), (c', w')) = Relation (c == c') w w'

negWords :: [String] 
negWords = ["not", "never", "isn't", "wasn't", "hasn't"]

negScope :: [String] -> [(Bool, Term)]
negScope = negScope' False
   where negScope' b (w : ws) | w `elem` negWords = negScope' True ws
                              | otherwise         = (b, w) : negScope' b ws
         negScope' _ [] = []

-- TODO: Serious implementation using synsets
synonymous :: Term -> Term -> Bool
synonymous = (==)

-- TODO: Serious implementation
antonymous :: Term -> Term -> Bool
antonymous _ _ = False
