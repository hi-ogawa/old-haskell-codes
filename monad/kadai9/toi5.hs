--monad
import Control.Monad.Error
import Control.Monad.State
--parsec
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language

lexer :: P.TokenParser ()
lexer = P.makeTokenParser (emptyDef{reservedOpNames = ["*","/","+","-", "<"]
                                   ,reservedNames =["if", "then", "else"]})

whiteSpace = P.whiteSpace lexer
lexeme = P.lexeme lexer
symbol = P.symbol lexer
natural = P.natural lexer
reserved = P.reserved lexer
reservedOp = P.reservedOp lexer
parens = P.parens lexer

expr = (do{ reserved "if"; e1 <- expr
          ; reserved "then"; e2 <- expr 
          ; reserved "else"; e3 <- expr
          ; return $ Eif e1 e2 e3})<|> exprOp

exprOp = buildExpressionParser table factor
       <?> "expression op"
       
table = [[op "*" Emul AssocLeft, op "/" Ediv AssocLeft]
        ,[op "+" Eplu AssocLeft, op "-" Esub AssocLeft]
        ,[op "<" Elt AssocLeft]]
  where op str func assoc
          = Infix (do{reservedOp str; return (\e1 e2-> func e1 e2)}) assoc
            
factor = parens expr <|> (do{i <- natural; return $ Eval (Vint i)})

runLex input = parse (do{ whiteSpace
                        ; x <- expr
                        ; eof
                        ; return x}) "test" input
               

mainTest input = case runLex input of               
  Left err -> print err
  Right e  -> mainEval e

data Exp = Eval Value
         | Eplu Exp Exp 
         | Esub Exp Exp
         | Emul Exp Exp    
         | Ediv Exp Exp
         | Elt  Exp Exp
         | Eif  Exp Exp Exp deriving (Show) 
data Value = Vint Integer | Vbool Bool deriving (Show)
    
-------ErrorT: 変換子-------------------
-- newtype ErrorT e m a = ErrorT { runErrorT :: m (Either e a) }    

-- instance (Monad m) => Functor (ErrorT e m) where
--     fmap f m = ErrorT $ do
--         a <- runErrorT m
--         case a of
--             Left  l -> return (Left  l)
--             Right r -> return (Right (f r))

-- instance (Monad m, Error e) => Monad (ErrorT e m) where
--     return a = ErrorT $ return (Right a)
--     m >>= k  = ErrorT $ do
--         a <- runErrorT m
--         case a of
--             Left  l -> return (Left l)
--             Right r -> runErrorT (k r)
--     fail msg = ErrorT $ return (Left (strMsg msg))

-- instance (Monad m, Error e) => MonadPlus (ErrorT e m) where
--     mzero       = ErrorT $ return (Left noMsg)
--     m `mplus` n = ErrorT $ do
--         a <- runErrorT m
--         case a of
--             Left  _ -> runErrorT n
--             Right r -> return (Right r)

-- instance (Monad m, Error e) => MonadError e (ErrorT e m) where
--     throwError l     = ErrorT $ return (Left l)
--     m `catchError` h = ErrorT $ do
--         a <- runErrorT m
--         case a of
--             Left  l -> runErrorT (h l)
--             Right r -> return (Right r)
            
-- instance (Error e) => MonadTrans (ErrorT e) where
--     lift m = ErrorT $ do
--         a <- m
--         return (Right a)

-- instance (Error e, MonadIO m) => MonadIO (ErrorT e m) where
--     liftIO = lift . liftIO

-- instance (Error e, MonadState s m) => MonadState s (ErrorT e m) where
--     get = lift get
--     put = lift . put            
---------------------------------

type MyError' = ErrorT String (Control.Monad.State.State Int)

mainEval :: Exp -> IO ()
mainEval exp = case runState (runErrorT $ eval' exp) 0 of
  (Left str, s) -> do putStrLn $ str++", ADD executed "++(show s)++" times."
  (Right v, s)  -> do putStrLn $ (show v)++", ADD executed "++(show s)++" times."

eval' :: Exp -> MyError' Value
eval' exp = case exp of
  Eval v        -> return v
  Eplu e1 e2    -> do{ v <- eval_math' e1 e2 (+)
                     ; s <- get
                     ; put (s+1)
                     ; return v}
  Esub e1 e2    -> eval_math' e1 e2 (-)
  Emul e1 e2    -> eval_math' e1 e2 (*)                 
  Ediv e1 e2    -> eval_math' e1 e2 (div)
  Elt  e1 e2    -> do{ i1 <- eval' e1 >>= getInt'; i2 <- eval' e2 >>= getInt'
                     ; return $ Vbool (i1 < i2)}
  Eif e1 e2 e3  -> do{ b1 <- eval' e1 >>= getBool'
                     ; v2 <- eval' e2; v3 <- eval' e3
                     ; return $ if b1 then v2 else v3}
                             
eval_math' :: Exp -> Exp -> (Integer -> Integer -> Integer) -> MyError' Value
eval_math' e1 e2 op = do{ i1 <- eval' e1 >>= getInt'
                        ; i2 <- eval' e2 >>= getInt'
                        ; if i2 == 0 && op 6 3 == 2
                          then throwError "ERROR: divide by zero"
                          else return $ Vint (op i1 i2)}
                      
getInt' :: Value -> MyError' Integer
getInt' (Vint i)  = return i
getInt' (Vbool _) = throwError "ERROR: expected 'Int'"

getBool' :: Value -> MyError' Bool
getBool' (Vbool b)  = return b
getBool' (Vint _)   = throwError "ERROR: expected 'Bool'"
