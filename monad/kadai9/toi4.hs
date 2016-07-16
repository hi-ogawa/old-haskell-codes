data Exp = Eval Value
         | Eplu Exp Exp 
         | Esub Exp Exp
         | Emul Exp Exp    
         | Ediv Exp Exp
         | Elt  Exp Exp
         | Eif  Exp Exp Exp deriving (Show) 
data Value = Vint Int | Vbool Bool
instance Show Value where
  show (Vint i) = show i
  show (Vbool b) = show b
data Return v = Err String | Val v
instance Show v => Show (Return v) where
  show (Err st) = "Error-Message: " ++ st
  show (Val val) = show val
  
instance Monad Return where
  return v = Val v
  Err st >>= f = Err st
  Val v >>= f = f v  

eval :: Exp -> Return Value
eval exp = case exp of
  Eval v              -> return v
  Eplu e1 e2          -> eval_math e1 e2 (+)
  Esub e1 e2          -> eval_math e1 e2 (-)
  Emul e1 e2          -> eval_math e1 e2 (*)
  Ediv e1 e2          -> eval_math e1 e2 (div)
  Elt e1 e2           -> do i1 <- eval e1 >>= getInt
                            i2 <- eval e2 >>= getInt
                            return (Vbool (i1 < i2))
  Eif e1 e2 e3        -> -- type[e2] must not to be equal type[e3]
    do b1 <- eval e1 >>= getBool
       (flip (>>=) return) $ if b1 then eval e2 else eval e3
  
eval_math :: Exp -> Exp -> (Int -> Int -> Int) -> Return Value
eval_math e1 e2 o = do i1 <- eval e1 >>= getInt
                       i2 <- eval e2 >>= getInt
                       if i2 == 0 && o 6 2 == 3 
                         then Err "divide by zero"
                         else return (Vint (o i1 i2))
  
getInt :: Value -> Return Int
getInt v = 
  case v of
    Vint i -> return i
    Vbool _ -> Err "expected 'Int', but comes 'Bool'."

getBool :: Value -> Return Bool
getBool v =
  case v of
    Vint _ -> Err "expected 'Bool', but comes 'Int'. "
    Vbool b -> return b
  

-- test expression
-- (Eplu (Eval $Vint 10) (Eval $Vint 15))
-- (Elt (Eval $Vint 5) (Eplu (Eval $Vint 10) (Eval $Vbool True)))
-- (Ediv (Eval $Vint 10) (Eval $Vint 0))
-- (Eif (Elt (Eval $Vint 5) (Eval $Vint 10)) (Eval $Vbool True) (Ediv (Eval $Vint 10) (Eval $Vint 0)))