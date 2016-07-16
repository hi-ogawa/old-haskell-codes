import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Error

type MyMonad = StateT Int (WriterT [String] (Either String))
-- きっとこんな感じ 
-- s -> Either String ((a, s ), w)

-- runしたらStateみえなくなるっぽ
testMain :: String -> Either String (String, [String])
testMain input = runWriterT (evalStateT (func input) 0)

func :: String -> MyMonad String
func input = do{ i <- get
               ; if i == 0
                 then do{ tell ["first <fun>"]
                        ; modify (+1)
                        ; func $ take ((length input) - 1) input}
                 else do{ tell ["second <fun>"]
                        ; i <- get
                        ; tell ["state: "++(show i)]
                        ; return input}}
                        -- ; throwError "this is error"}}
                        

                