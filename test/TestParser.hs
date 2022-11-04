-- | Performs testing on the parser.
module TestParser (parserTests) where

import AST
import Files (Path)
import Parser
import Test.HUnit

parserTests :: Test
parserTests = TestList [testText, testHeader, testEquation, testEnvironment]

testText :: Test
testText =
  let actual = runParserOn "Basic test, $1 + 1 = 2$ and $n! = \\prod{n;k=1}k$."
      expected =
        ( show
            [ Text "Basic test, ",
              Inline [Text "1 + 1 = 2"],
              Text "and ",
              Inline [Text "n! = ", Macro "prod" [Text "n", SemiColon, Text "k=1"], Text "k"],
              Text "."
            ]
        )
   in TestCase (assertEqual "Text parsing, " actual expected)

testHeader :: Test
testHeader =
  let actual = runParserOn "# About the factorial $n! = \\prod{n;k=1}k$."
      expected =
        ( show
            [ Header
                1
                [ Text " About the factorial ",
                  Inline [Text "n! = ", Macro "prod" [Text "n", SemiColon, Text "k=1"], Text "k"],
                  Text "."
                ]
            ]
        )
   in TestCase (assertEqual "Header parsing, " actual expected)

testEquation :: Test
testEquation =
  let actual = runParserOn "@eq{recursive factorial}:\n\tn! &:= \\prod{n; k = 1}k\n\t&= n \\cdot (n-1)!\n"
      expected =
        show
          ( [ Equation
                [Text "recursive factorial"]
                [ Text "n! ",
                  Ampersand,
                  Text ":= ",
                  Macro
                    "prod"
                    [Text "n", SemiColon, Text " k = 1"],
                  Text "k",
                  Ampersand,
                  Text "= n ",
                  Macro "cdot" [],
                  Text " (n-1)!"
                ]
            ]
          )
   in TestCase (assertEqual "Equation parsing, " actual expected)

testEnvironment :: Test
testEnvironment =
  let actual = runParserOn "@def{factorial}:\n\tWe define the factorial of $n \\in \\N$ as $n! = \\prod{n; k=1}k$.\n\tsecond line.\n" -- TODO: Make this test more interesting.
      expected =
        show
          ( [ Environment
                "def"
                [Text "factorial"]
                [ Text "We define the factorial of ",
                  Inline
                    [Text "n ", Macro "in" [], Text " ", Macro "N" []],
                  Text " as ",
                  Inline [Text "n! = ", Macro "prod" [Text "n", SemiColon, Text " k=1"], Text "k"],
                  Text ".",
                  Text "second line."
                ]
            ]
          )
   in TestCase (assertEqual "Environment parsing, " actual expected)
