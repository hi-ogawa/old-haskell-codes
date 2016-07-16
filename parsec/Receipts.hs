import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import Data.Char (digitToInt)
import Control.Monad (liftM)

type Receipt = ([Product], Total)
type Total = Int
data Product = Ret Int | Buy String Int deriving Show

langDef = emptyDef {reservedNames   = ["return", "total"] 
                   ,reservedOpNames = ["*", "/", "+", "-"]}
lexer = P.makeTokenParser (langDef)

whiteSpace = P.whiteSpace lexer
lexeme = P.lexeme lexer
identifier = P.identifier lexer
reserved = P.reserved lexer
semi = P.semi lexer

receipt :: Parser Receipt
receipt = do ps <- many productMy
             to <- total
             return $ (ps, to)
             
productMy :: Parser Product
productMy = do retOrBuy <- ret <|> buy
               i <- price
               semi
               return $ retOrBuy i
ret :: Parser (Int -> Product)
ret = do reserved "return"
         return $ Ret
buy = liftM Buy identifier

price :: Parser Int
price = lexeme (do{ ds1 <- many1 digit
                  ; char '.'
                  ; ds2 <- count 2 digit
                  ; return (convert 0 (ds1 ++ ds2))})
        <?> "price"
        where convert n [] = n
              convert n (d:ds) = convert (10*n + digitToInt d) ds
              
total :: Parser Int
total = do i <- price
           reserved "total"
           return i
           
runLex p input = parse (do{ whiteSpace
                          ; re <- receipt
                          ; eof
                          ; return re}) "receipt" input
                 
---------------------------------------------------------------------- 

type Token = (SourcePos, Tok)
data Tok   = Identifier String
           | Reserved   String
           | Symbol     String
           | Price      Int
           deriving Show
scanner :: [Char] -> ([Token], [String])                    
                    
--(REFERENCE) Parser は文字列に対して動作する状態を持たないパーサの型シノニム
--type Parser a = GenParser Char () a
type MyParser a = GenParser Token () a

--token :: (t -> String) -> (t -> SourcePos) -> (t -> Maybe a) -> Text.Parsec.Prim.Parsec s u a

mytoken :: (Tok -> Maybe a) -> MyParser a
mytoken test = token showToken posToken testToken
  where showToken (pos,tok) = show tok
        posToken (pos,tok)  = pos
        testToken (pos,tok) = test tok
        
identifier' :: MyParser String
identifier' = mytoken (\tok -> case tok of 
                          Identifier name -> Just name
                          other           -> Nothing)
              
