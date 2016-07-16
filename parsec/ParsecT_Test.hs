-- parser libs --
import Text.Parsec.Prim (ParsecT, runParserT)
import Text.Parsec.Expr

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Control.Applicative ((<$>), pure, (<*>), (<*), (*>))
-----------------

import qualified Data.Map as Mp
import Control.Monad.Trans (lift)
import qualified Control.Monad.State as CMS

type Id = String

data Exp = IntE Int
         | VarE Id
         | AddE Exp Exp
         | MulE Exp Exp
           deriving Show

type ParserM a = ParsecT String () (CMS.State Int) a

genFresh :: ParserM Int
genFresh = lift $ CMS.modify (+1) >> CMS.get

doParse :: ParserM a -> String -> Either ParseError a
doParse p inp = flip CMS.evalState 0 $ runParserT (whiteSpace *> p <* eof) () "" inp

pExp :: ParserM Exp
pExp = buildExpressionParser operators pSimpleExp
       <?> "expresion"
  where operators = [[opCOp (r_op "+", AddE, AssocLeft), opCOp (r_op "*", MulE, AssocLeft)]]
        opCOp (sym, c, ass) = Infix (sym *> pure c) ass
       
pSimpleExp :: ParserM Exp
pSimpleExp = IntE <$> integer
             <|> pVar
             <|> parens pExp
             <?> "simple-expression"
             
pVar :: ParserM Exp
pVar = pure (\id i -> VarE (id ++ show i)) <*> identifier <*> genFresh

-- lexer --
lexer :: P.GenTokenParser String () (CMS.State Int)
lexer = P.makeTokenParser langDef

langDef :: P.GenLanguageDef String () (CMS.State Int)
langDef = P.LanguageDef { P.commentStart    = "(*",
                          P.commentEnd      = "*)",
                          P.commentLine     = "--",
                          P.identStart      = lower,
                          P.identLetter     = lower <|> digit <|> upper,
                          P.opStart         = oneOf (concat reservedOperators),
                          P.opLetter        = oneOf (concat reservedOperators),
                          P.reservedNames   = reservedKeywords,
                          P.reservedOpNames = reservedOperators,
                          P.nestedComments  = True,
                          P.caseSensitive   = True }

reservedKeywords = []

reservedOperators = ["+", "*"]

r_op  = P.reservedOp lexer
r_op' = P.symbol lexer

integer :: ParserM Int
integer = fromIntegral <$> P.natural lexer

whiteSpace :: ParserM ()
whiteSpace = P.whiteSpace lexer

parens :: ParserM a -> ParserM a
parens = P.parens lexer

identifier :: ParserM String
identifier = P.identifier lexer
