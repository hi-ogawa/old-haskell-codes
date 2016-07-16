import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language

lexer :: P.TokenParser ()
lexer = P.makeTokenParser (haskellDef {reservedOpNames = ["*","/","+","-"]})
lexer = P.makeTokenParser (emptyDef)

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer
lexeme :: Parser a -> Parser a
lexeme = P.lexeme lexer
symbol = P.symbol lexer
natural :: Parser Integer
natural = P.natural lexer
parens = P.parens lexer
semi = P.semi lexer
identifier :: Parser String
identifier = P.identifier lexer
reserved = P.reserved lexer
reservedOp = P.reservedOp lexer

expr :: Parser Integer
expr = buildExpressionParser table factor
       <?> "expression"
       
table = [[op "*" (*) AssocLeft, op "/" div AssocLeft]
        ,[op "+" (+) AssocLeft, op "-" (-) AssocLeft]]
  where op str func assoc
          = Infix (do{reservedOp str; return func}) assoc
            
factor = parens expr <|> natural            
         <?> "simple expression"
         

run p input = parse p "test" input
runLex p input = run (do{ whiteSpace
                        ; x <- p
                        ; eof
                        ; return x}) input
