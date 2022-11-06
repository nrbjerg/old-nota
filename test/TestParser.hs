{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

-- | Performs testing on the parser.
module TestParser (parserTests) where

import AST
import Files (Path)
import Parser
import Test.HUnit

parserTests :: Test
parserTests = TestList [testText, testHeader, testEnvironment, testEquation]

testText :: Test
testText =
  let actual = runParserOn "Basic test, $1 + 1 = 2$ and i hate my self. % Yes yes"
      expected =
        ( show
            [ Text "Basic test, ",
              Inline [Element "1", Operand '+', Element "1", Operand '=', Element "2"],
              Text " and i hate my self. ",
              Comment
            ]
        )
   in TestCase (assertEqual "Text parsing, " actual expected)

testHeader :: Test
testHeader =
  let actual = runParserOn "# About the factorial $n! = \\prod^n_{k=1} k$."
      expected =
        ( show
            [ Header
                1
                [ Text " About the factorial ",
                  Inline
                    [ Element "n",
                      Operand '!',
                      Operand '=',
                      Macro "prod" [],
                      Raised (Element "n"),
                      Subscript (Group [Element "k", Operand '=', Element "1"]),
                      Element "k"
                    ],
                  Text "."
                ]
            ]
        )
   in TestCase (assertEqual "Header parsing, " actual expected)

testEquation :: Test -- FIXME:
testEquation =
  let actual = runParserOn "@eq{generator_function}:\n\tG_n^\\pi(s)&=\\E(s^{Z_n} | E_n) &=  \\sum_j s^j {P(Z_n = j, E_n)}/{P(E_n)} " -- "&= {G_n(s\\eta) - G_n(0)} / {\\eta - G_n(0)}"
      expected =
        show
          ( [ Equation
                [Text "generator_function"]
                [ Element "G",
                  Subscript $ Element "n",
                  Raised $ Macro "pi" [],
                  BracketPair '(' [Element "s"] ')',
                  Ampersand,
                  Operand '=',
                  Macro "E" [],
                  BracketPair
                    '('
                    ( [ Element "s",
                        Raised $ Group [Element "Z", Subscript $ Element "n"],
                        Operand '|',
                        Element "E",
                        Subscript $ Element "n"
                      ]
                    )
                    ')',
                  Ampersand,
                  Operand '=',
                  Macro
                    "sum"
                    [],
                  Subscript $ Element "j",
                  Element "s",
                  Raised $ Element "j",
                  Fraction
                    ( Group
                        ( [ Element "P",
                            BracketPair
                              '('
                              ( [ Element "Z",
                                  Subscript $ Element "n",
                                  Operand '=',
                                  Element "j",
                                  Operand ',',
                                  Element "E",
                                  Subscript $ Element "n"
                                ]
                              )
                              ')'
                          ]
                        )
                    )
                    ( Group
                        ( [ Element "P",
                            BracketPair
                              '('
                              ( [ Element "E",
                                  Subscript $ Element "n"
                                ]
                              )
                              ')'
                          ]
                        )
                    )
                ]
            ]
          )
   in TestCase (assertEqual "Equation parsing, " actual expected)

testEnvironment :: Test
testEnvironment =
  let actual = runParserOn "@def{factorial}:\n\tWe define the factorial of $n \\in \\N$ as $n! = \\prod^n_{k=1}k$.\n\tsecond line.\n" -- TODO: Make this test more interesting.
      expected =
        show
          ( [ Environment
                "def"
                [Text "factorial"]
                [ Text "We define the factorial of ",
                  Inline
                    [Element "n", Macro "in" [], Macro "N" []],
                  Text " as ",
                  Inline
                    [ Element "n",
                      Operand '!',
                      Operand '=',
                      Macro "prod" [],
                      Raised (Element "n"),
                      Subscript (Group [Element "k", Operand '=', Element "1"]),
                      Element "k"
                    ],
                  Text ".",
                  Text "second line."
                ]
            ]
          )
   in TestCase (assertEqual "Environment parsing, " actual expected)
