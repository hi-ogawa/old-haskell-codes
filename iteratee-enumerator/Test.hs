import Control.Applicative (Applicative, pure, (<*>))
import Data.Maybe (catMaybes)
import System.IO --(Handle, hIsEOF, hGetChar, openFile, hClose, IOMode(ReadMode))
import Control.Exception (bracket)
import Control.Monad ((>=>))
import Control.Monad.Trans (MonadTrans, lift, MonadIO, liftIO)

-- reference [ http://d.hatena.ne.jp/tanakh/20100824#p1 ] --

data StreamG el = Empty | El el | EOF

data IterV el a = Done a (StreamG el)
                | Cont (StreamG el -> IterV el a)
                  
-- foldlみたいな何か(Doneの処理を追加した感じ) --
-- 値を与える
enum :: IterV el a -> [el] -> IterV el a
enum i            []     = i
enum i@(Done _ _) _      = i
enum (Cont k)     (x:xs) = enum (k (El x)) xs

-- EOFを送り計算した値を取り出す
run :: IterV el a -> Maybe a
run (Done x _)  = Just x
run (Cont k)    = run' (k EOF)
  where run' (Done x _) = Just x
        run' _          = Nothing
        
-- examples --
myId :: IterV el [el]
myId = Cont (step [])
  where step acc (El e) = Cont (step (acc ++ [e]))
        step acc Empty  = Cont (step acc)
        step acc EOF    = Done acc EOF
        
myHead :: IterV el (Maybe el)
myHead = Cont step
  where step (El e)     = Done (Just e) Empty
        step Empty      = Cont step
        step EOF        = Done Nothing EOF
        
myPeek :: IterV el (Maybe el)
myPeek = Cont step
  where step c@(El e)   = Done (Just e) c
        step Empty      = Cont step
        step EOF        = Done Nothing EOF
        
myDrop :: Int -> IterV el ()
myDrop 0 = Done () Empty
myDrop n = Cont step
  where step (El e) = myDrop (n-1)
        step Empty  = Cont step
        step EOF    = Done () EOF
        
myLength :: IterV el Int        
myLength = Cont (step 0)
  where step acc (El e) = Cont (step (acc+1))
        step acc Empty  = Cont (step acc)
        step acc EOF    = Done acc EOF

instance Monad (IterV el) where
  return x = Done x Empty
  m >>= f  = 
    case m of
      Done x str  -> 
        case f x of
          Done x' _     -> Done x' str
          Cont k'       -> k' str
      Cont k      -> 
        Cont (\str -> k str >>= f)

instance Functor (IterV el) where
  fmap f (Done x str)   = Done (f x) str
  fmap f (Cont k)       = Cont (fmap f . k)

instance Applicative (IterV el) where
  pure x = Done x Empty
  (Done f str) <*> i2   = fmap f i2
  (Cont k)     <*> i2   = Cont (\str -> k str <*> i2)

    
drop1keep1 :: IterV el (Maybe el)
drop1keep1 = myDrop 1 >> myHead
  
alternates5 :: IterV el [el]
alternates5 = fmap catMaybes . sequence . replicate 5 $ drop1keep1

-- これはダメっぽ(停止しない) --
alternates :: IterV el [el]
alternates = fmap catMaybes . sequence . repeat $ drop1keep1

-- enumerator for input --
-- iterに入力を与えたitarateeを返す
type EnumeratorM el m a = IterV el a -> m (IterV el a)

enumHandle :: Handle -> EnumeratorM Char IO a
enumHandle h iter = loop iter
  where loop i@(Done _ _)       = return i
        loop i@(Cont k)         = 
          do isEOF <- hIsEOF h
             if isEOF 
               then return i
               else hGetChar h >>= loop . k . El
                    
enumFile :: FilePath -> EnumeratorM Char IO a
enumFile fname iter = 
  bracket 
  (openFile fname ReadMode) 
  (hClose) 
  (flip enumHandle iter)

lengthOfTwoFiles :: FilePath -> FilePath -> IO (Maybe Int)
lengthOfTwoFiles f1 f2 =
  fmap run $ (enumFile f1 >=> enumFile f2) myLength

-- for output --
data IterVM el m a = DoneM a (StreamG el)
                   | ContM (StreamG el -> Iteratee el m a)
                     
newtype Iteratee el m a 
  = Iteratee {runIter :: m (IterVM el m a)}
    
-- my definition --
-- enumM :: Monad m => Iteratee el m a -> [el] -> m (Iteratee el m a)
-- enumM i []      = return i
-- enumM i (x:xs)  = runIter i >>= step
--   where step i@(DoneM _ _)      = return . Iteratee . return $ i
--         step (ContM k)          = enumM (k (El x)) xs
        
-- is this better than above def ?? --
enumM :: Monad m => Iteratee el m a -> [el] -> Iteratee el m a
enumM i []      = i
enumM i (x:xs)  = Iteratee $ runIter i >>= step
  where step i@(DoneM _ _)      = return i
        step (ContM k)          = runIter $ enumM (k (El x)) xs

runM :: Monad m => Iteratee el m a -> m (Maybe a)
runM i = runIter i >>= step 
  where step (DoneM x _)        = return $ Just x
        step (ContM k)          = runM' (k EOF)
        runM' i'                = runIter i' >>= step'
        step' (DoneM x _)       = return $ Just x
        step' (ContM _)         = return $ Nothing
-- my definition --

liftIter :: Monad m => IterV el a -> Iteratee el m a
liftIter (Done x str)   = Iteratee . return $ DoneM x str
liftIter (Cont k)       = Iteratee . return $ ContM (liftIter . k)

instance MonadTrans (Iteratee el) where
  lift m = Iteratee $ m >>= \x -> return (DoneM x Empty)

-- my definition--
instance Monad m => Monad (Iteratee el m) where
  return x = Iteratee . return $ DoneM x Empty
  mo >>= f = Iteratee $ runIter mo >>= step1
    where step1 (ContM k)       = return $ ContM (\str -> k str >>= f)
          step1 (DoneM x str)   = runIter (f x) >>= step2
            where step2 (DoneM x' _)    = return $ DoneM x' str
                  step2 (ContM k')      = runIter $ k' str
-- my definition--          

instance MonadIO m => MonadIO (Iteratee el m) where
  liftIO = lift . liftIO

streamToFile :: FilePath -> Iteratee Char IO ()
streamToFile fname = Iteratee (openFile fname WriteMode >>= go)
  where go h = return $ ContM (step h)
        step h (El el)  = Iteratee (hPutChar h el >> go h)
        step h Empty    = Iteratee (go h)
        step h EOF      = Iteratee (return $ DoneM () EOF)
        
-- examples --
-- runM $ enumM (streamToFile "test.out") ['a'..'z']    -- 書き込み
        
-- let altM = liftIter alternates5 :: Iteratee el IO [el]
-- runM $ enumM altM [1..]            => Just [2,4,6,8,10]


-- for map and filter --
        
-- type EnumerateeM elOuter elInner m a =
--   Iteratee elInner m a
--   -> Iteratee elOuter m (Iteratee elInner m a)
  
-- myFilter :: (el -> Bool) -> IterV el a -> IterV el (IterV el a)
-- myFilter pred i@(Done _ _) = return i
-- myFilter pred (Cont k)     = Cont step
--   where step (El e) | pred e    = myFilter pred (k e)
--         step EOF                = Done (k EOF) EOF
--         step _                  = Cont step
        

-- mapIter :: (elOuter -> elInner)
--            -> IterV elInner a
--            -> IterV elOuter (IterV elInner a)
-- mapIter f i@(Done _ _) = return i
-- mapIter f (Cont k) = Cont step
--   where
--     step (El el) = mapIter f (k (El $ f el))
--     step Empty = Cont step
--     step EOF = Done (k EOF) EOF