module Parse where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Expr as E
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (LanguageDef, emptyDef)

-- main --
parseMain :: String -> Either ParseError Term
parseMain input = runParse pMain input

-- test run --
runParse :: Parser a -> String -> Either ParseError a
runParse p input = parse (do spaces
                             e <- p
                             eof
                             return e) "runParse" input
        
-- input --
pMain :: Parser Term
pMain = pTerm                    

-- term syntax --
pTerm :: Parser Term
pTerm = pApplyOrSimple
        
pApplyOrSimple :: Parser Term
pApplyOrSimple = E.buildExpressionParser ops pSimpleTerm
  where ops = [ [E.Infix (return (\tm1 tm2 -> App tm1 tm2)) E.AssocLeft] ]

pSimpleTerm :: Parser Term
pSimpleTerm = pPrimC <|> pVar <|> pAbs <|> parensL pTerm

pPrimC :: Parser Term
pPrimC = PrimCL `fmap` (try (IntL `fmap` naturalL) <|> BoolL `fmap` bool)
  where bool = try (symbolL "#t" >> return True) <|> (symbolL "#f" >> return False)

pVar :: Parser Term
pVar = Var `fmap` identifierL

pAbs :: Parser Term
pAbs = do symbolL "\\"
          id <- identifierL
          -- symbolL ":"
          -- ty <- pType
          symbolL "."
          tm <- pTerm
          return $ Abs (id, Nothing) tm

-- lexer --
langDef :: LanguageDef st
langDef = emptyDef { P.identStart      = lower
                   , P.identLetter     = lower <|> digit
                   , P.caseSensitive   = True}
          
lexer :: P.TokenParser ()
lexer = P.makeTokenParser langDef

parensL         = P.parens lexer    
symbolL         = P.symbol lexer    
identifierL     = P.identifier lexer    
naturalL        = fromIntegral `fmap` do char '#'; P.natural lexer
