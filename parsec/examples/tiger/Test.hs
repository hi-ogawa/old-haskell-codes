import Text.ParserCombinators.Parsec -- hiding (spaces)
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language

lexer = P.makeTokenParser emptyDef

parens          = P.parens lexer    
whiteSpace      = P.whiteSpace lexer    
symbol          = P.symbol lexer    
identifier      = P.identifier lexer    
integer         = P.integer lexer

-- expr    = term   `chainl1` addop
-- term    = factor `chainl1` mulop
factor  = parens expr <|> integer

-- mulop   =   do{ symbol "*"; return (*)   }
--             <|> do{ symbol "/"; return (div) }
            
-- addop   =   do{ symbol "+"; return (+) }
--             <|> do{ symbol "-"; return (-) }
            

expr = buildExpressionParser tableMD term
term = buildExpressionParser tableAS factor

tableMD = [[Infix (do{ symbol "*"; return (*)}) AssocLeft
           ,Infix (do{ symbol "/"; return (div)}) AssocLeft]]
          
tableAS = [[Infix (do{ symbol "+"; return (+)}) AssocLeft
           ,Infix (do{ symbol "+"; return (+)}) AssocLeft]] 
          
            
runLex :: Parser a -> String -> Either ParseError a
runLex p input = parse (do{ whiteSpace
                          ; e <- p
                          ; eof
                          ; return e}) "test" input
            