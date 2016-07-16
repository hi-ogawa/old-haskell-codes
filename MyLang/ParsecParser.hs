module ParsecParser where

-- parser libs --
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P
import Control.Applicative ((<$>), pure, (<*>), (<*), (*>))
-----------------

import Types
import Control.Monad.Trans (lift)
import Control.Monad.State (StateT, get, gets, put, modify, evalStateT)

type TVarMap = [(String, Type)]
type ParserM a = ParsecT String () (StateT TVarMap LangM) a


doParseMain :: String -> LangM ([TypeInfo], Exp)
doParseMain = doParse $ pure (,) <*> many pDefineType <*> pExp

doParseExp :: String -> LangM Exp
doParseExp = doParse pExp

doParse :: ParserM a -> String -> LangM a
doParse p inp = either (throwLangError . ParseEr) return =<< 
                (flip evalStateT [] $ runParserT (whiteSpace *> p <* eof) () "" inp) -- :: LangM (Either ParseError a)

pExp :: ParserM Exp
pExp = pLet
       <|> pIf
       <|> pAbs
       <|> pPreInfixExp -- cons, op, app
       <|> pCase
       <?> "expression"

pSimpleExp :: ParserM Exp
pSimpleExp = IntE <$> integer
             <|> BoolE <$> bool
             <|> pure AssertE <* r_error
             <|> VarE <$> pIdentifier
             <|> pUserE
             <|> pList
             <|> pure IOGetcE <* r_getChar
             <|> try (pure UnitE <* r_op "()")
             <|> try pTupple
             <|> parens pExp
             <?> "simple-expression"

pList :: ParserM Exp
pList = foldr ConsE NilE <$> brackets (sepBy pExp (r_op ","))

pTupple :: ParserM Exp
pTupple = TupE <$> parens (sepBy2 pExp (r_op ","))

pLet :: ParserM Exp
pLet = pure LetE
       <* r_let <*> sepBy1 pBind r_and <* r_in <*> pExp

pBind :: ParserM (String, Exp)
pBind = do id:args <- many1 pIdentifier
           r_op "="
           ex <- pExp 
           return (id, foldr (\arg acc -> AbsE arg acc) ex args)

pUserE :: ParserM Exp
pUserE = UserE <$> pCapIdentifier
       
        
pPreInfixExp :: ParserM Exp
pPreInfixExp = buildExpressionParser operators pSimpleExp
               <?> "infix-or-prefix-expression"
  where operators = [[Infix (whiteSpace *> pure AppE) AssocLeft],
                     [Prefix (r_op "-" *> pure (OpE MinusC (IntE 0)))],
                     [Prefix (r_not *> pure notByIf)],
                     [Prefix (r_return *> pure IORetE), Prefix (r_putChar *> pure IOPutcE)],
                     [opCOp (r_op "*", MulC, AssocLeft),  opCOp (r_op "/", DivC, AssocLeft)],
                     [opCOp (r_op "+", PlusC, AssocLeft), opCOp (r_op "-", MinusC, AssocLeft)],
                     [opCOp (r_op "==",  EqC, AssocNone), opCOp (r_op "/=", NeqC, AssocNone),
                      opCOp (r_op "<",  LtC, AssocNone),  opCOp (r_op "<=", LeC, AssocNone),
                      opCOp (r_op ">",   GtC, AssocNone), opCOp (r_op ">=", GeC, AssocNone)],
                     [Infix (r_op ":" *> pure ConsE) AssocRight],
                     [Infix (r_op ">>=" *> pure IOBindE) AssocLeft]
                    ]
        opCOp (sym, opc, ass) = Infix (sym *> pure (OpE opc)) ass
        notByIf b = IfE b (BoolE False) (BoolE True)

pIf :: ParserM Exp
pIf = pure IfE 
      <* r_if <*> pExp <* r_then <*> pExp <* r_else <*> pExp
         
pAbs :: ParserM Exp
pAbs = pure AbsE 
       <* r_op "\\" <*> pIdentifier <* r_op "->" <*> pExp

pCase :: ParserM Exp
pCase = pure CaseE 
        <* r_match <*> pExp
        <* r_with <*> (many1 (r_op "|" *> pPatAndExp))
        
pPatAndExp :: ParserM (Pattern, Exp)
pPatAndExp = pure (,)
             <*> pPat <* r_op "->" <*> pExp
             
-- pattern
pPat :: ParserM Pattern
pPat = buildExpressionParser operators pSimplePat
  where operators = [[Infix (r_op' ":" *> pure ConsP) AssocRight]]

pSimplePat :: ParserM Pattern
pSimplePat = IntP <$> integer
             <|> BoolP <$> bool
             <|> pListPat
             <|> VarP <$> pIdentifier
             <|> pure WildP <* r_op' "_"
             <|> try (pure UnitP <* r_op "()")
             <|> try pTupPat
             <|> parens pPat
             <|> pUserPat
       
pListPat :: ParserM Pattern
pListPat = foldr ConsP NilP <$> brackets (sepBy pPat (r_op ","))

pTupPat :: ParserM Pattern
pTupPat = TupP <$> parens (sepBy2 pPat (r_op ","))

pUserPat :: ParserM Pattern
pUserPat = pure UserP 
           <*> pCapIdentifier <*> option [] (braces (sepBy1 pPat (r_op ",")))
           
-- type
pDefineType :: ParserM TypeInfo
pDefineType = pure (,)
              <* r_data <*> pUserDec <* r_op "=" <*> sepBy1 (pure (,) <*> pCapIdentifier <*> many pType) (r_op "|")

pUserDec :: ParserM Type
pUserDec = UserT <$> pCapIdentifier <*> option [] (do ids <- braces (many pIdentifier) -- declaration of type variable
                                                      tys <- sequence . take (length ids) $ repeat freshTVar
                                                      lift $ put (zip ids tys)
                                                      return tys)
  where freshTVar :: ParserM Type
        freshTVar = VarT <$> (lift$lift$freshGen)

pType :: ParserM Type
pType = buildExpressionParser operators pSimpleType
  where operators = [[Infix (pure FunT <* r_op "->") AssocRight]]


pSimpleType :: ParserM Type
pSimpleType = try (pure IntT <* r_Int)
              <|> try (pure BoolT <* r_Bool)
              <|> (idToTVar =<< pIdentifier)
              <|> pUserType
              <|> pListType
              <|> try (pure UnitT <* r_op "()")
              <|> try pTupType
              <|> parens pType

pListType :: ParserM Type
pListType = ListT <$> brackets pType

pTupType :: ParserM Type
pTupType = TupT <$> parens (sepBy2 pType (r_op ","))

pUserType :: ParserM Type
pUserType = UserT <$> pCapIdentifier <*> option [] (braces (many pType))
        
idToTVar :: String -> ParserM Type -- resolution of type variable
idToTVar a = maybe (lift$lift$throwLangError (TypeEr $ "not in scope type variable: "++a)) return =<< (lift $ gets (lookup a))

-- lexer --
lexer :: P.GenTokenParser String () (StateT TVarMap LangM)
lexer = P.makeTokenParser langDef

langDef :: P.GenLanguageDef String () (StateT TVarMap LangM)
langDef = P.LanguageDef { P.commentStart    = "(*",
                          P.commentEnd      = "*)",
                          P.commentLine     = "--",
                          P.identStart      = upper <|> lower,
                          P.identLetter     = lower <|> digit <|> upper,
                          P.opStart         = oneOf (concat reservedOperators),
                          P.opLetter        = oneOf (concat reservedOperators),
                          P.reservedNames   = reservedKeywords,
                          P.reservedOpNames = reservedOperators,
                          P.nestedComments  = True,
                          P.caseSensitive   = True }

reservedKeywords = ["let", "in", "and",
                    "if", "then", "else",
                    "match", "with",
                    "True", "False",
                    "error",
                    "data",
                    "Int", "Bool",
                    "not",
                    "return",
                    "putChar",
                    "getChar"
                   ]

reservedOperators = [":",
                     "=",
                     "|", 
                     "\\", "->", 
                     "+", "-", "/", "*",
                     "==", "/=", "<", "<=", ">", ">=",
                     ",",
                     "_",
                     ">>=",
                     "()"
                    ]

reserved = P.reserved lexer
r_op  = P.reservedOp lexer
r_op' = P.symbol lexer

r_let        = reserved "let"
r_in         = reserved "in"
r_and        = reserved "and"
r_if         = reserved "if"
r_then       = reserved "then"
r_else       = reserved "else"
r_match      = reserved "match"
r_with       = reserved "with"
r_True       = reserved "True"
r_False      = reserved "False"
r_error      = reserved "error"
r_data       = reserved "data"
r_Int        = reserved "Int"
r_Bool       = reserved "Bool"
r_not        = reserved "not"
r_return     = reserved "return"
r_putChar    = reserved "putChar"
r_getChar    = reserved "getChar"


integer :: ParserM Int
integer = fromIntegral <$> P.natural lexer

bool :: ParserM Bool
bool = (r_True >> return True)
        <|> (r_False >> return False)

whiteSpace :: ParserM ()
whiteSpace = P.whiteSpace lexer

parens :: ParserM a -> ParserM a
parens = P.parens lexer

braces :: ParserM a -> ParserM a
braces = P.braces lexer

brackets :: ParserM a -> ParserM a
brackets = P.brackets lexer

identifier :: ParserM String
identifier = P.identifier lexer

pIdentifier :: ParserM String
pIdentifier = try $
              do str <- identifier
                 if isCapital str
                   then unexpected "identifier"
                   else return str

pCapIdentifier :: ParserM String
pCapIdentifier = try $
                 do str <- identifier
                    if isCapital str
                      then return str
                      else unexpected "constructor"

isCapital :: String -> Bool
isCapital []    = error "ParsecParser.hs - isCapical"
isCapital (c:_) = 'A' <= c && c <= 'Z'



-- combinator not defined in lib --
sepBy2 :: ParserM a -> ParserM b -> ParserM [a]
sepBy2 p s = pure (\x y zs -> (x:y:zs))
             <*> p <* s <*> p <*> many (s *> p)