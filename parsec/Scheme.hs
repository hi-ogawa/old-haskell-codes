-- REFERENCE
-- http://ja.wikibooks.org/wiki/48%E6%99%82%E9%96%93%E3%81%A7Scheme%E3%82%92%E6%9B%B8%E3%81%93%E3%81%86/%E6%A7%8B%E6%96%87%E8%A7%A3%E6%9E%90

import Text.ParserCombinators.Parsec -- hiding (spaces)
import Control.Monad

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
               deriving Show
                        
-- parse :: Parser a -> String(name) -> String(input) -> Either ParseError a            
-- skipMany1 :: Text.Parsec.Prim.ParsecT s u m a -> Text.Parsec.Prim.ParsecT s u m ()   
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"
-- spaces :: Parser ()
-- spaces = skipMany space

readtest input = parse (spaces >> symbol) "" input
-- (REFERENCE) Parserモナドでは、bindは「一つ目のパーサのマッチを試み、二つ目のパーサを残りの入力に対してマッチを試み、どちらかがマッチに失敗したら失敗する」という意味です。
-- noneOf :: [Char] -> Text.Parsec.Prim.ParsecT s u m Char
-- many :: Text.Parsec.Prim.ParsecT s u m a  -> Text.Parsec.Prim.ParsecT s u m [a]
parseString :: Parser LispVal
parseString = do char '"'       -- (")自体は読み捨てる
                 x <- many (noneOf "\"")
                 char '"'
                 return $ String x
                 
-- (REFERENCE) returnはそれを包み上げ、入力を何も消費せずそれを内部の値として返すParserアクションにしてくれます。                 
-- oneOf :: [Char] -> Text.Parsec.Prim.ParsecT s u m Char
-- (<|>) :: Text.Parsec.Prim.ParsecT s u m a -> Text.Parsec.Prim.ParsecT s u m a -> Text.Parsec.Prim.ParsecT s u m a                 
parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol                       -- first :: Char
               rest <- many (letter <|> digit <|> symbol)       -- rest  :: String
               let atom = first:rest
               return $ case atom of
                 "#t" -> Bool True
                 "#f" -> Bool False
                 _    -> Atom atom
parseNumber = liftM (Number . read) $ many1 digit

-- sepBy :: Text.Parsec.Prim.ParsecT s u m a -> Text.Parsec.Prim.ParsecT s u m sep -> Text.Parsec.Prim.ParsecT s u m [a]
parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces         -- parseExprと相互再帰
parseDottedList = do head <- endBy parseExpr spaces
                     tail <- char '.' >> spaces >> parseExpr
                     return $ DottedList head tail
parseQuoted = do char '\''
                 x <- parseExpr
                 return $ List [Atom "quote", x]
                 
-- try :: GenParser tok st a -> GenParser tok st a  (これはparseListのバックトラックのため)
parseExpr = parseAtom 
            <|> parseString 
            <|> parseNumber 
            <|> parseQuoted
            <|> do char '('
                   e <- try parseList <|> parseDottedList       -- 相互再帰
                   char ')'
                   return e

readExpr :: String -> Either ParseError LispVal
readExpr input = parse parseExpr "lisp" input

{- test case
% ./simple_parser "(a test)"
Found value
% ./simple_parser "(a (nested) test)"
Found value
% ./simple_parser "(a (dotted . list) test)"
Found value
% ./simple_parser "(a '(quoted (dotted . list)) test)"
Found value
% ./simple_parser "(a '(imbalanced parens)"
No match: "lisp" (line 1, column 24):
unexpected end of input
expecting space or ")"
-}