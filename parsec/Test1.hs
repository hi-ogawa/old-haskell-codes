import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
-- import qualified Text.ParserCombinators.Parsec.Token as P
-- import Text.ParserCombinators.Parsec.Language
import Control.Monad

run :: Parser a -> String -> Either ParseError a
run parser input = parse parser "test" input

expr :: Parser Integer
expr = buildExpressionParser table factor
       <?> "expression"
       
table = [[op "*" (*) AssocLeft, op "/" div AssocLeft]
        ,[op "+" (+) AssocLeft, op "-" (-) AssocLeft]]
  where op s f assoc
          = Infix (do{ string s; return f}) assoc
            
factor :: Parser Integer
factor = do{ char '('; e <- expr; char ')'; return e}
         <|> number
         <?> "simple expression"
         
number :: Parser Integer
number = liftM (read) (many1 digit)
         <?> "number"