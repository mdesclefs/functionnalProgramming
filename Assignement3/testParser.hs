import qualified Parser

test :: String -> String -> [(String, String)]
test re str = Parser.apply (Parser.parser exp' re) str

test_color = (if test "red" "red" == [(Red, "")] then "Passed" else "Failed") ++ " (test 'red' 'red')\n"