module Parser where

-- import Syntax
import Text.ParserCombinators.Parsec -- hiding (spaces)
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import Data.List(delete)
import Control.Monad(liftM)

data Expr = IntLit Integer
          | FloatLit Double
          | BoolLit Bool
          | EOp Op Expr Expr
          | EUnOp Op Expr
          | Eif Expr Expr Expr
          | Elet Var Expr Expr
          | Evar Var
          | EletF Var [Var] Expr Expr
          | Eapp Var [Expr]            
          | Etuple [Expr]
          | EletTuple [Var] Expr Expr
          | Array Expr Expr
          | Get Expr Expr
          | Put Expr Expr Expr
          deriving Show
data Op = Add | Sub | Mul | Div 
        | Eq | Ne | Lt | Gt | Le | Ge
        | FAdd | FSub | FMul | FDiv deriving Show
type Var = String  

langDef :: LanguageDef st
langDef = emptyDef { P.commentStart    = "(*"
                   , P.commentEnd      = "*)"
                   , P.nestedComments  = True
                   , P.reservedNames   = ["if", "then", "else", "let", "in", "let rec"
                                         ,"true", "false", "Array.create", "&-", "."]
                   , P.reservedOpNames = ["+", "-", "*", "/"
                                         ,"+.", "-.", "*.", "/."
                                         ,"=", "<>", "<=", ">=", "<", ">"]
                   , P.opLetter        = oneOf (concat $ P.reservedOpNames langDef)
                   , P.caseSensitive   = True}
lexer :: P.TokenParser ()
lexer = P.makeTokenParser langDef

commaSep1       = P.commaSep1 lexer
parens          = P.parens lexer    
whiteSpace      = P.whiteSpace lexer    
symbol          = P.symbol lexer    
identifier      = P.identifier lexer    
reserved        = P.reserved lexer    
reservedOp      = P.reservedOp lexer
integer         = P.integer lexer
float           = P.float lexer

commaSep2 :: Parser a -> Parser [a]
commaSep2 p = do{ a1 <- p; symbol ","; as <- commaSep1 p; return $ [a1] ++ as}

-- :: Parser Expr
-- 先頭の[+, -]をパースしない
intLiteral      = do{ notFollowedBy $ reservedOp "+"; notFollowedBy $ reservedOp "-"; i <- integer; return $ IntLit i} <?> "literal"
floatLiteral    = do{ notFollowedBy $ reservedOp "+"; notFollowedBy $ reservedOp "-";f <- float; return $ FloatLit f} <?> "literal"
boolLiteral     = (do{ reserved "true"; return $ BoolLit True} 
                   <|> do{ reserved "false"; return $ BoolLit False}) <?> "literal"
variable        = do{ id <- identifier; return $ Evar id} <?> "literal"
                  
funCallExpr     = do{ func <- identifier    
                    ; args <- many1 simpleNcallExpr     -- ここ重要
                    ; return $ Eapp func args}
              
ifExpr          = do{ reserved "if"; e1 <- expr
                    ; reserved "then"; e2 <- expr
                    ; reserved "else"; e3 <- expr
                    ; return $ Eif e1 e2 e3}
letExpr         = do{ reserved "let"; id <- identifier
                    ; reservedOp "="; e1 <- expr
                    ; reserved "in"; e2 <- expr
                    ; return $ Elet id e1 e2}
letRecExpr      = do{ reserved "let rec"; func <- identifier
                    ; args <- many1 identifier
                    ; reservedOp "="; e1 <- expr
                    ; reserved "in"; e2 <- expr
                    ; return $ EletF func args e1 e2}
tupleExpr       = liftM Etuple $ parens $ commaSep2 expr
letTupleExpr    = do{ reserved "let"; vars <- parens $ commaSep2 identifier
                    ; reservedOp "="; e1 <- expr
                    ; reserved "in"; e2 <- expr
                    ; return $ EletTuple vars e1 e2}
arrayExpr       = do{ reserved "Array.create"
                    ; e1 <- expr; e2 <- expr
                    ; return $ Array e1 e2}
                  
getExpr = do{ s <- simpleNcallNgetExpr; reserved "."
            ; es <- sepBy1 (parens expr) (reserved ".") 
            ; return $ foldl Get s es}
          
-- simpleNcallExpr中でgetExprが呼ばれて、[.(expr)]が全部食われてしまう。一つだけ残したいけど、難しい。
putExpr = do{ s <- simpleNcallExpr; reserved "."
-- putExpr = do{ s <- simpleNcallNgetExpr; reserved "."
-- putExpr = do{ s <- getExpr; reserved "."
            ; e1 <- parens expr; reserved "&<"
            ; e2 <- expr; return $ Put s e1 e2}
          
infixExpr :: Parser Expr
infixExpr = buildExpressionParser operators simpleExpr
operators = [ [prefix "-" Sub, prefix "-." FSub]
            , map (\(n,f) -> op n f AssocLeft) [("*", Mul), ("/", Div), ("*.", FMul), ("/.", FDiv)]
            , map (\(n,f) -> op n f AssocLeft) [("+", Add), ("-", Sub), ("+.", FAdd), ("-.", FSub)]
            , map (\(n,f) -> op n f AssocNone) [("=", Eq), ("<>", Ne), ("<=", Le), (">=", Ge), ("<", Lt), (">", Gt)]
            ]
  where op name func assoc = Infix (do {reservedOp name; return (\e1 e2 -> EOp func e1 e2)}) assoc
        prefix name func = Prefix (do {reservedOp name; return (\e -> EUnOp func e)})
                           
simpleExpr = choice [ try floatLiteral
                     , intLiteral
                     , boolLiteral
                     , try $ getExpr                       
                     , try $ parens expr
                     , try funCallExpr
                     , variable
                     ] <?> "simple expression"
simpleNcallNgetExpr = choice [ try floatLiteral
                             , intLiteral
                             , boolLiteral
                             , variable
                             , parens expr
                             ] <?> "simple expression without call and get"
simpleNcallExpr = choice [ try floatLiteral
                         , intLiteral
                         , boolLiteral
                         , parens expr
                         , try getExpr          -- callより結合が強い
                         , variable
                         ] <?> "simple expression without call"

expr :: Parser Expr     -- 上の6個は先読みすれば一意
expr = choice [ ifExpr         
              , letRecExpr
              , letExpr
              , letTupleExpr
              , arrayExpr
              , try tupleExpr                
              , try putExpr     -- ??
              , infixExpr      -- infixEpxr に simpleExprが含まれる                
              ] <?> "expression"
       
runLex :: Parser a -> String -> Either ParseError a
runLex p input = parse (do{ whiteSpace
                          ; e <- p
                          ; eof
                          ; return e}) "min-caml" input

{- test code
"let rec fib x = if x <= 1 then x else fib(x-1) + fib(x-2) in print_int (fib 15)"

"let rec inprod v1 v2 i = if i < 0 then 0.0 else  v1.(i) *. v2.(i) +. inprod v1 v2 (i - 1) in let v1 = Array.create 3 1.23 in let v2 = Array.create 3 4.56 in print_int (truncate (1000000. *. inprod v1 v2 2))"

-}