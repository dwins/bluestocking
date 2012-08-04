module ParserTest where

import Parse
import Test.HUnit

testMakeRelations, testStripNeg, testIsNeg :: Test
testMakeRelations = TestList [
        testMake "one negated term"   (True,  "foo") (False, "bar") False,
        testMake "zero negated terms" (False, "foo") (False, "bar") True,
        testMake "two negated terms"  (True,  "foo") (True,  "bar") True
    ] where testMake s a b expected = 
              TestCase $ assertEqual s expected $ co (mkRelation (a, b))

testStripNeg = TestList [
        testStrip "With negative" (True, "foo") "foo",
        testStrip "Without negative" (False, "foo") "foo"
    ] where testStrip s a expected = TestCase $ assertEqual s expected $ stripNeg a

testIsNeg = TestList [
        testCheck "With negative" (True, "foo") True,
        testCheck "Without negative" (False, "foo") False
    ] where testCheck s a expected = TestCase $ assertEqual s expected $ isNeg a
 

allTests :: Test
allTests = TestList [
        testMakeRelations,
        testStripNeg,
        testIsNeg
    ]

main :: IO Counts
main = runTestTT allTests
