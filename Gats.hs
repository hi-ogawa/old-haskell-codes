{-# LANGUAGE GADTs #-}
import Control.Monad.State
import Control.Applicative ((<*>), pure)
import Data.IORef
import System.IO.Unsafe

data Exp a where
  EInt  :: Int  -> Exp Int
  EBool :: Bool -> Exp Bool
  EAdd  :: Exp Int -> Exp Int -> Exp Int
  EEq   :: (Eq a) => Exp a -> Exp a -> Exp Bool
  EIf   :: Exp Bool -> Exp a -> Exp a -> Exp a
  EAbs  :: (Exp a -> Exp b) -> Exp (a -> b)
--  EVar  :: String -> Exp a
--  ELet  :: Exp a -> 
  EApp  :: Exp (a -> b) -> Exp a -> Exp b
  EPlace:: a -> Exp a      --for eval
  EVar  :: String -> Exp a --for ppe

{- reduction example
eval $ EApp (EAbs (\x -> EAdd x x)) (EApp (EAbs (\x -> EAdd x x)) (EApp (EAbs (\x -> EAdd x x)) (EInt 3)))
-}

eval :: Exp a -> a  
eval e =
  case e of
    EInt i       -> i
    EBool b      -> b
    EAdd e1 e2   -> eval e1 + eval e2
    EEq e1 e2    -> eval e1 == eval e2
    EIf eb e1 e2 -> if eval eb then eval e1 else eval e2
    EAbs f       -> \x -> eval $ f (EPlace x)
    EApp f arg   -> (eval f) (eval arg)
    EPlace x     -> x




type Fresh a = State Int a

ppeMain e = evalState (ppe e) 0

ppe :: Exp a -> Fresh String
ppe e =
  r p <*>
  case e of
    EInt i       -> r $ show i
    EBool b      -> r $ show b
    EAdd e1 e2   -> psM [ppe e1, r "+",  ppe e2]
    EEq e1 e2    -> psM [ppe e1, r "==", ppe e2]
    EIf eb e1 e2 -> psM [r "if", ppe eb, r "then", ppe e1, r "else", ppe e2]
    EAbs f       -> do str <- genFresh
                       psM [r ("\\" ++ str ++" -> "), ppe $ f (EVar str)]
    EApp f arg   -> psM [ppe f, ppe arg]
    EPlace x     -> error "gomi"
    EVar s       -> r s
 where p s = " "++s++" "
       psM = fmap concat . sequence
       r = return
       genFresh :: Fresh String
       genFresh = do i <- get
                     put (i+1)
                     return ("x" ++ show i)

-- ppe (EAbs (\x -> EAdd x (EInt 10)))
--            ^^f^^

-- f (EVar "x")
-- => EAdd (EVar "x") (EInt 10)
-- => "x" + 10

ref :: IORef Int
ref = unsafePerformIO $ newIORef 0

ppe' :: Exp a -> String
ppe' e =
  p $
  case e of
    EInt i       -> show i
    EBool b      -> show b
    EAdd e1 e2   -> ppe' e1 ++ "+"  ++ ppe' e2
    EEq e1 e2    -> ppe' e1 ++ "==" ++ ppe' e2
    EIf eb e1 e2 -> "if" ++ ppe' eb ++ "then" ++ ppe' e1 ++ "else" ++ ppe' e2
    EAbs f       -> let str = genFresh ()
                    in "\\" ++ str ++" -> " ++ ppe' (f (EVar str))
    EApp f arg   -> ppe' f ++ ppe' arg
    EPlace x     -> error "gomi"
    EVar s       -> s
   where  p s = " "++s++" "

genFresh :: () -> String
genFresh () = let i = unsafePerformIO $ do i <- readIORef ref
                                           writeIORef ref (i+1)
                                           return i
              in "x" ++ show i
