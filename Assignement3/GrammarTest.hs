import qualified Parser
import qualified Control.Monad

data Expr = Constant Int
          | Variable String
          | Plus Expr Expr
          | Lambda [String] Expr
          | Call Expr [Expr]
          deriving Show

parenth :: Parser.Parser a -> Parser.Parser a
parenth p = do Parser.keyword "("
               result <- p
               Parser.keyword ")"
               return result

expr :: Parser.Parser Expr
expr = Parser.oneof [constant, variable, plus, lambda, call]

constant :: Parser.Parser Expr
constant = Control.Monad.liftM Constant Parser.fromRead

variable :: Parser.Parser Expr
variable = Control.Monad.liftM Variable Parser.token

plus :: Parser.Parser Expr
plus = parenth $ Control.Monad.liftM2 Plus expr (Parser.keyword "+" >> expr)

lambda :: Parser.Parser Expr
lambda = parenth $ Parser.keyword "lambda" >> Control.Monad.liftM2 Lambda (Parser.many Parser.token) expr

call :: Parser.Parser Expr
call = parenth $ Control.Monad.liftM2 Call expr (Parser.many expr)

main :: IO ()
main = do
        printStrLn $ Parser.apply (Parser.parser expr "")  "((lambda x (x + 1)) 2)"
