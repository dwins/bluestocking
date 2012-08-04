module GeneralTest where

import Parse
import Doxament
import Test.HUnit


knowledgeBase :: Doxament
knowledgeBase = merge a b
    where a = relations "Today was a good day. Yesterday was a bad day."
          b = relations "Ice cream is good. Spinach is bad. Today was not bad."

quickQuery :: Document -> (Score, Contradictions)
quickQuery = query knowledgeBase . relations

assertQueryAbove, assertQueryBelow :: String -> Score -> Assertion
assertQueryAbove q threshold =
    assertBool label (s > threshold)
       where s = score (query knowledgeBase $ relations q)
             label = "Query score not above " ++ show threshold ++ ": " ++ show s

assertQueryBelow q threshold =
    assertBool label (s < threshold)
       where s = score (query knowledgeBase $ relations q)
             label = "Query score not below " ++ show threshold ++ ": " ++ show s

assertNonEmpty :: [a] -> Assertion
assertNonEmpty d = assertBool "Was empty" (not $ null d)

assertEmpty :: Show a => [a] -> Assertion
assertEmpty d = assertBool ("Was not empty" ++ show d) (null d)

assertNegative :: (Show a, Num a, Ord a) => String -> a -> Assertion
assertNegative label x = assertBool (label ++ " (" ++ show x ++ ") was not negative") (x < 0)

assertPositive :: (Show a, Num a, Ord a) => String -> a -> Assertion
assertPositive label x = assertBool (label ++ " (" ++ show x ++ ") was not positive") (x > 0)

testQueries :: Test
testQueries = TestList [test1, test2, test3]
    where 
       test1 = let (s, _) = quickQuery "Today was a good day because I ate ice cream"
               in TestCase $ assertPositive "Score" s
       test2 = let text = "Yesterday was good because I ate spinach. Today was bad."
                   (s, _) = quickQuery text
               in TestLabel text $ TestCase $ assertNegative "Score" s
       test3 = let text = "Today was not good. Ice cream is not good. Spinach is bad. I hate everything."
                   (s, contras) = quickQuery text
               in TestLabel text $ TestCase $ 
                   do assertNegative "Score" s
                      assertBool ("Today is good should be in " ++ show contras ) $ corpusAgree contras $ Relation True "Today" "good"

testRelations :: Test
testRelations = TestList [ 
        testOn "Today was a good day because I ate ice cream"
    ] where testOn s = TestCase $ assertNonEmpty $ relations s

testParses :: Test
testParses = TestList [
        testOn sentences,
        testOn relations
    ] where testOn f = TestCase $ assertNonEmpty $ f "Today was a good day because I ate ice cream"

allTests :: Test
allTests = TestList [ testParses, testRelations, testQueries ]

main :: IO Counts
main = runTestTT allTests

-- main :: IO ()
-- main = mapM_ print knowledgeBase
