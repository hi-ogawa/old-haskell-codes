import Control.Monad.State
import Control.Monad.Error

type MyMonad = ErrorT Err (State Int)
data Err = Err1 
         | Err2 
         | Err3
           deriving Show

instance Error Err where
  noMsg         = Err1
  strMsg str    = Err1

test :: String -> MyMonad Char
test input = do{ modify (+(length input))
               ; if (length input) `mod` 2 == 0
                 then do { c <- myError `catchError` myHandler
                         ; return c }
                 else do { c <- myErrorDummy `catchError` myHandler
                         ; return c }}
  
myHandler :: Err -> MyMonad Char
myHandler err = case err of
  Err1  -> return '1'
  Err2  -> return '2'
  Err3  -> return '3'

myError :: MyMonad Char
myError = do{ throwError Err2
            ; return 'e' }

myErrorDummy :: MyMonad Char
myErrorDummy = do{ modify (+1)
                 ; return 'd' }