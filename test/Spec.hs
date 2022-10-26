-- \| Perform testing.

import Test.HUnit
import TestParser

tests :: Test
tests = TestList [parserTests]

main :: IO ()
main = runTestTTAndExit tests
