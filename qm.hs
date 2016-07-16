import Data.List (nub, elemIndices, sortBy, groupBy)
import Data.Array
import Data.IORef
import Control.Arrow ((&&&))

-- parser library --
import Text.ParserCombinators.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (emptyDef)
import Control.Applicative ((<$>))
--------------------


type Id = String
data Fml = Var Id
         | Not Fml
         | And Fml Fml
         | Or Fml Fml
         deriving (Show, Eq)

data Bit = I
         | O
         | D
         deriving (Show, Eq)
                  
type Bits = [Bit]
  
type Row = (Bits, [Int], Bool)

type Grps = [(Int, [Row])]

type Table = ([Id], [(Bits, Bit)])

--qmMain :: Table -> 
qmMain (vars, table) = undefined
        
--genInitRow :: [(Bits, Bit)] -> Grps
genInitRow table = map (fst . head &&& map snd) $ initGrp
  where initGrp = groupBy eqf . sortBy ordf . map ((length . elemIndices I &&& id) . fst) . filter ((==I) . snd) $ table :: [[(Int, Bits)]]
        ordf t t' = compare (fst t) (fst t')
        eqf  t t' = (fst t) == (fst t')
        


testfml = "(p /\\ q) \\/ (r /\\ (s \\/ ~ q))"
Right (vars, table) = getDNF `fmap` runParseFml testfml

-- parse and print truth table
maintest :: String -> IO ()
maintest str = either print (\(ids, table) -> print ids >> mapM_ print table)
               $ getDNF `fmap` runParseFml str


-- generating truth table
getDNF :: Fml -> Table
getDNF fml = (vars, [ (showJ j vars, addj' fml j) | j <- valuations])
  where valuations = ps vars :: [[Id]]
        vars = nub . gather $ fml :: [Id]
        gather :: Fml -> [Id]
        gather f =
          case f of
            Var x     -> [x]
            Not f1    -> gather f1
            And f1 f2 -> gather f1 ++ gather f2
            Or  f1 f2 -> gather f1 ++ gather f2
        showJ :: [Id] -> [Id] -> [Bit]
        showJ j vars = [ if elem v j then I else O | v <- vars ]
        addj' :: Fml -> [Id] -> Bit
        addj' f j | addj f j  = I
                  | otherwise = O
        addj :: Fml -> [Id] -> Bool
        addj f j =
          case f of
            Var x     | elem x j  -> True
                      | otherwise -> False
            Not f1    -> not $ addj f1 j
            And f1 f2 -> addj f1 j && addj f2 j
            Or  f1 f2 -> addj f1 j || addj f2 j
        ps :: [a] -> [[a]]
        ps []     = [[]]
        ps (x:xs) = concatMap (\s -> [s, x:s]) $ ps xs



--------------------
-- ltl-fml parser --
--------------------
runParseFml :: String -> Either ParseError Fml        
runParseFml = doParse parseFml
        
doParse :: Parser a -> String -> Either ParseError a  
doParse p inp = parse (whiteSpace >> p) "fml-parser" inp

parseFml :: Parser Fml
parseFml = buildExpressionParser operators parseSimpleFml
           <?> "top-fml"
  where operators = [ map (\(op,f) -> prefixOp op f) [(opNot, Not)],
                      map (\(op,f) -> infixOp op f AssocLeft) [(opAnd, And), (opOr, Or)]]
        infixOp op func assoc = Infix (op >> return func) assoc
        prefixOp op func = Prefix (op >> return func)

parseSimpleFml :: Parser Fml
parseSimpleFml = (Var <$> identifier)
                 <|> parens parseFml
                 <?> "simple-fml"

lexer :: P.TokenParser st
lexer = P.makeTokenParser langDef

langDef :: P.LanguageDef st
langDef = emptyDef { P.identStart = lower,
                     P.identLetter= lower,
                     P.reservedOpNames = ["/\\", "\\/", "~"]}

reservedOp = P.reservedOp lexer

opAnd = reservedOp "/\\"
opOr  = reservedOp "\\/"
opNot = reservedOp "~"

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer

parens :: Parser a -> Parser a
parens = P.parens lexer

identifier :: Parser String
identifier = P.identifier lexer
