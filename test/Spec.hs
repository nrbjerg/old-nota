import AST
import Files (Path)
import Parser

main :: IO ()
main = do
  testBasics
  testHeader
  testEnvironment

testBasics :: IO ()
testBasics =
  if testParser "Basic test, $1 + 1 = 2$ and $n! = \\prod{n,k=1}k$."
    == ( show
           [ Text "Basic test",
             Comma,
             Inline [Text "1 + 1 = 2"],
             Text "and ",
             Inline [Text "n! = ", Macro "prod" [Text "n", Comma, Text "k=1"], Text "k"],
             Text "."
           ]
       )
    then putStrLn "Basic parser test succeeded"
    else putStrLn "Basic parser test failed"

testHeader :: IO ()
testHeader =
  if testParser "# About the factorial $n! = \\prod{n,k=1}k$."
    == ( show
           [ Header
               1
               [ Text " About the factorial ",
                 Inline [Text "n! = ", Macro "prod" [Text "n", Comma, Text "k=1"], Text "k"],
                 Text "."
               ]
           ]
       )
    then putStrLn "Header test succeeded"
    else putStrLn "Header test failed"

testEnvironment :: IO ()
testEnvironment =
  if testParser "@def*{factorial}:\n   We define the factorial of $n \\in \\N$ as $n! = \\prod{n, k=1}k$."
    == ( show
           [ Environment
               "def"
               True
               [Text "factorial"]
               [ Text "We define the factorial of ",
                 Inline [Text "n ", Macro "in" [], Text " ", Macro "N" []],
                 Text "as ",
                 Inline [Text "n! = ", Macro "prod" [Text "n", Comma, Text " k=1"], Text "k"]
               ]
           ]
       )
    then putStrLn "Environment test succeeded"
    else putStrLn "Environment test failed"
