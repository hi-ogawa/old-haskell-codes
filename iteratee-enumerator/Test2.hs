-- reference [http://research.preferred.jp/2011/02/enumerator-package-yet-another-iteratee-tutorial/] --
import Prelude hiding (head)
import Control.Exception.Base (SomeException, throw)
-- import Data.Function (fix)
import Data.List (splitAt)
import Control.Monad.Trans (MonadIO, liftIO, lift, MonadTrans)
import System.IO
import Control.Exception (bracket)

-- fundamental type definition --
data Stream a = Chunks [a] | EOF        -- Chunk [] が Empty に相当する

instance Functor Stream where
  fmap f EOF            = EOF
  fmap f (Chunks es)    = Chunks (fmap f es)
  
data Step a m b = Continue (Stream a -> Iteratee a m b)
                | Yield b (Stream a)
                | Error SomeException
                  
newtype Iteratee a m b = Iteratee {runIteratee :: m (Step a m b)}

type Enumerator a m b = Step a m b -> Iteratee a m b

instance Monad m => Monad (Iteratee a m) where
  return x      = yield x (Chunks [])   -- YieldにおけるEmptyとEOFの違いは大きいぞ！(bind(>>=)に注目)
  mo >>= f      = 
    Iteratee $ do step1 <- runIteratee mo
                  case step1 of
                    Continue k           -> return $ Continue (\str -> k str >>= f)
                    e@(Error err)        -> return (Error err)  -- なぜかeでreturnできない??
                    Yield x str          ->
                      do step2 <- runIteratee $ f x
                         case step2 of
                           Continue k'       -> runIteratee $ k' str
                           e'@(Error _)      -> return e'
                           Yield x' _        -> return $ Yield x' str   -- ここに食べ残しは存在し得ない(step1のstrが残ってる)
                           
instance MonadTrans (Iteratee a) where
  lift m = Iteratee $ m >>= \x -> return (Yield x (Chunks []))

instance MonadIO m => MonadIO (Iteratee a m) where                           
  liftIO = lift . liftIO

---------------------
-- helper function --
---------------------
                   
-- Step を Iteratee にそのまま変換
returnI :: Monad m => Step a m b -> Iteratee a m b
returnI = Iteratee . return

yield :: Monad m => b -> Stream a -> Iteratee a m b
yield acc str = returnI $ Yield acc str
  
continue :: Monad m => (Stream a -> Iteratee a m b) -> Iteratee a m b
continue = returnI . Continue 

-- EOFを送って結果を返してもらう
run :: Monad m => Iteratee a m b -> m (Either SomeException b)
run itee =
  runIteratee itee >>= 
  \step -> case step of
    Continue k          -> run $ k EOF -- if misbehaving itee, goes inf loop (手抜き)
    Error err           -> return $ Left err
    Yield res str       -> return $ Right res
    
run_ :: Monad m => Iteratee a m b -> m b    
run_ itee = run itee >>= either throw return

-------------------------
-- example of Iteratee --
-------------------------
    
head :: Monad m => Iteratee a m (Maybe a)
head = continue go
  where go (Chunks [])          = continue go
        go (Chunks (x:xs))      = yield (Just x) (Chunks xs)    -- そもそも食われなかった分は知らんってゆう
        go EOF                  = yield Nothing EOF

-- 重要なのは[go(Step)]が何をやっているか
sum5 :: Monad m => Iteratee Int m Int
sum5 = continue (go 0)
  where go acc (Chunks ints)    = continue (go (acc + sum ints))
        go acc EOF              = yield acc EOF
        
-- 実際はMonadicに書ける
sum6 :: Monad m => Iteratee Int m Int
sum6 = do mayInt <- head
          case mayInt of
            Nothing     -> return 0
            Just i      -> do rest <- sum6
                              return $ i + rest
                              
---------------------------
-- example of enumerator --
---------------------------

enumList :: Monad m => Int -> [a] -> Enumerator a m b
enumList n = loop
  where loop ls (Continue k) | not (null ls) = let (s1, s2) = splitAt n ls 
                                               in k (Chunks s1) >>== loop s2
        loop _ step                          = returnI step
        
        
infixr 0 $$
infixl 1 ==<<
infixl 1 >>==

-- enumeratorで入力をiterateeに渡した段階でContinueなのか途中でYieldしたのかどっちかになっている
-- 例えば
-- run_ $ enumList 2 [12..20] $$ enumList 2 [1..11] $$ sum6
-- のような場合、
-- enumList 2 [1..11] $$ sum6
-- の段階で、まだ食べれますよアピールのContinueなイテレータが返ってきて
-- enumList 2 [12..20]
-- を、続けて食べる。
-- もちろんこれを食べ終えてもContinueなので、最終的には
-- run_
-- で入力の終わりEOFを送り、Iterateeとして計算してきた値を返してもらえる。

-- (==<<) :: Monad m => Enumerator a m b -> Iteratee a m b -> Iteratee a m b
(==<<) :: Monad m => (Step a m b -> Iteratee a' m b) -> Iteratee a m b -> Iteratee a' m b
(==<<) enum itee = 
  Iteratee $ do step <- runIteratee itee
                runIteratee $ enum step

-- ($$) :: Monad m => Enumerator a m b -> Iteratee a m b -> Iteratee a m b
($$) :: Monad m => (Step a m b -> Iteratee a' m b) -> Iteratee a m b -> Iteratee a' m b
($$) = (==<<)

-- (>>==) :: Monad m => Iteratee a m b -> Enumerator a m b -> Iteratee a m b
(>>==) :: Monad m => Iteratee a m b -> (Step a m b -> Iteratee a' m b) -> Iteratee a' m b
(>>==) itee enum =
  Iteratee $ do step <- runIteratee itee
                runIteratee $ enum step


-- 標準入力からenum --
getNumberEnum :: MonadIO m => Enumerator Int m b
getNumberEnum (Continue k)      = 
  do x <- liftIO getLine
     case x of
       "q"      -> continue k
       _        -> k (Chunks [read x]) >>== getNumberEnum
getNumberEnum step              = returnI step

getWordEnum :: MonadIO m => Enumerator String m b
getWordEnum (Continue k) =
  do x <- liftIO getLine
     case x of
       "q"      -> continue k
       _        -> k (Chunks $ words x) >>== getWordEnum

-- ファイルからenum(open, closeをちゃんとしてる!) --
fileEnum :: FilePath -> Enumerator Char IO b
fileEnum fname step =
  do h <- liftIO $ openFile fname ReadMode      --open
     handleEnum h step

handleEnum :: Handle -> Enumerator Char IO b
handleEnum h (Continue k)       =
  do bEof <- liftIO $ hIsEOF h
     if bEof 
       then do liftIO $ hClose h        -- close
               continue k
       else do c <- liftIO $ hGetChar h
               k (Chunks [c]) >>== handleEnum h
handleEnum h step               = 
  do liftIO $ hClose h  -- close
     returnI step

-- 標準出力へ一行づつ吐き出すiteratee --
iteePrint :: (MonadIO m, Show a) => Iteratee a m ()
iteePrint = 
  do maya <- head
     case maya of
       Nothing  -> return ()
       Just a   -> do liftIO $ print a
                      iteePrint
        

-- map iteratee --
mapIter :: Monad m => (ao -> ai) -> Iteratee ai m b -> Iteratee ao m b
mapIter f it =
  Iteratee $ do step <- runIteratee it
                return $ go step -- go :: Step ai m b -> Step ao m b
  where go (Yield res str)      = Yield res EOF -- ??なんか、そのままstr返せば良い気がするけど(んー型的にダメか) ?? ここらへんの確実な処理のためにあの恐怖の型が出来上がっているのかもな
        go (Error err)          = Error err
        go (Continue k)         = Continue (\str -> Iteratee $ do step' <- runIteratee $ k (f `fmap` str)
                                                                  return $ go step')

-- test :: IO Int
-- test = run_ (enumList 1 ["1", "2", "3", "4"] $$ (read `mapIter` sum6))

-- but in the package definition --
-- type Enumeratee ao ai m b = Step ai m b -> Iteratee ao m (Step ai m b)
-- map :: Monad m => (ao -> ai) -> Step ai m b -> Iteratee ao m (Step ai m b)

unnest :: Monad m => Iteratee ao m (Step ai m b) -> Iteratee ao m b
-- unnest :: Monad m => Iteratee String m (Step Int m Int) -> Iteratee String m Int
unnest outer = do innerStep <- outer
                  go innerStep  -- go :: Step ai m b -> Iteratee ao m b
  where go (Yield res _)        = return res
        go e@(Error err)        = undefined -- returnI e
        -- k :: Stream ai -> Iteratee ai m b
        go (Continue k)         = k EOF >>== go
        
-- (>>==) :: Monad m => Iteratee a m b -> Enumerator a m b -> Iteratee a m b

-- test = run_ $ 