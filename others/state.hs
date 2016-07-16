import Data.Char
import Data.Bits
import Control.Monad.State

data B64EncState = First | Second Int | Third Int
table = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "+/"

-- procChar :: B64EncState -> Int -> (B64EncState, String)
-- procChar First  c = (Second (c .&. 3), [(!!) table (shiftR c 2))])
-- procChar (Second i) c = (Third (c .&. 15), [(!!) table ((shiftL i 4) .|. (shiftR c 4))])
-- procChar (Third i) c = (First, map (table!!) [((shiftL i 2) .|. (shiftR c 6))
--                                               , (c .&. 63)])


procChar :: Int -> State B64EncState String
procChar c = get >>= transit
  where transit First       = do put (Second (c .&. 3))
                                 return [table !! shiftR c 2]
        transit (Second i)  = do put (Third (c .&. 15))
                                 return [table !! ((shiftL i 4) .|. (shiftR c 4))]
        transit (Third i)   = do put First
                                 return (map (table!!) [((shiftL i 2) .|. (shiftR c 6)) , (c .&. 63)])
                              

--procChar . ord  ::  Char -> State B64ES String
--mapM            ::  (Char -> State B64ES String) -> [Char] -> State B64ES [String]
                                
-- [State B64ES String] -> 
                                
-- b64Enc s = do strs <- mapM (procChar . ord) s
--               return (concat strs)
                                 
-- runState testFunc1 (Right "")                                
-- State { runState :: TestState -> (String, TestState)                                 
                                 
data TestState = TUp String | TRight String | TDown String | TLeft String deriving Show                                
                                 
-- State sFunc                                 
-- sFunc :: TestState -> (String, TestState)
-- sFunc TUp = ("TUp", TRight)
-- sFunc TRight = ("TRight", TUp)
                                 


testFunc1 :: State TestState String
testFunc1 = get >>= transit
  where transit (TUp s)    = do put (TRight (s++"U"))
                                return (s++"U")
        transit (TRight s) = do put (TDown (s++"R"))
                                return (s++"R")
        transit (TDown s)  = do put (TLeft (s++"D"))
                                return (s++"D")
        transit (TLeft s)  = do put (TUp (s++"U"))
                                return (s++"U")

          