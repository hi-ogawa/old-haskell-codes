{-# LANGUAGE DoRec #-}

module EvalLazy where

import Types

import qualified Data.Map as Mp
import Data.List (intersperse)
--import Control.Monad (foldM, msum, when)
--import Control.Monad.Trans (lift)
--import Data.IORef
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.State
import Control.Applicative (pure, (<*>), (<$>))
import Data.Maybe (fromJust)
import Data.Char (digitToInt, intToDigit)

-- evalMain :: Exp -> IO ()
-- evalMain exp = either print return =<< runLangMT (eval exp)
-- 
-- eval :: Exp -> EvalM ()
-- eval exp = topPrint =<< reduce exp (VEnv Mp.empty)
-- 
-- topPrint :: Whnf -> EvalM ()
-- topPrint ob =
--   case ob of
--     IntO i           -> p $ show i
--     BoolO b          -> p $ show b
--     FunO _ _ _       -> p "*function*"
--     NilO             -> p "[]"
--     ConsO e1 e2 venv -> do topPrint =<< reduce e1 venv
--                            p ":"
--                            topPrint =<< reduce e2 venv
--     TupO es venv     -> do p "(" 
--                            sequence_ . intersperse (p ", ") . map (\e -> topPrint =<< reduce e venv) $ es
--                            p ")"
--   where p s = lift $ putStr s
-- 
-- reduce :: Exp -> VEnv -> EvalM Whnf
-- reduce exp venv =
--   case exp of
--     LetE xes e2  -> reduce e2 =<< letBind xes venv
--     VarE x       -> veLookup x venv
--     AbsE x e     -> return $ FunO x e venv
--     AppE e1 e2   -> do (x, e, venv') <- funCheck =<< reduce e1 venv
--                        reduce e =<< veInsert x (e2, venv) venv'
--     IntE i       -> return $ IntO i
--     BoolE b      -> return $ BoolO b
--     NilE         -> return $ NilO
--     ConsE e1 e2  -> return $ ConsO e1 e2 venv
--     TupE es      -> return $ TupO es venv
--     CaseE e1 pes ->
--       (maybe (throwEvalError "EVAL:pattern match failer") return =<<) . runMaybeT . msum
--       $ map (\(pat, e) -> do ve <- patMatch (e1, venv) venv pat
--                              lift $ reduce e ve
--             ) pes
--     IfE e1 e2 e3 -> 
--       do b <- boolCheck =<< reduce e1 venv
--          if b then reduce e2 venv else reduce e3 venv
--     OpE o e1 e2 ->
--       do i1 <- intCheck =<< reduce e1 venv
--          i2 <- intCheck =<< reduce e2 venv
--          return $ case o of
--            PlusC   -> IntO (i1 + i2)
--            MinusC  -> IntO (i1 - i2)
--            EqC     -> BoolO (i1 == i2)
--            LtC     -> BoolO (i1 < i2)
--     AssertE      -> throwEvalError "asserting"
--     
-- patMatch :: (Exp, VEnv) -> VEnv -> Pattern -> MaybeT EvalM VEnv
-- patMatch _       venv WildP    = return venv
-- patMatch ev      venv (VarP x) = lift $ veInsert x ev venv
-- patMatch (e, ve) venv pat =
--   do w <- lift $ reduce e ve
--      case (w, pat) of
--        (IntO i, IntP i')   | i == i' -> return venv
--        (BoolO b, BoolP b') | b == b' -> return venv
--        (NilO, NilP)                  -> return venv
--        (ConsO e1 e2 v, ConsP p1 p2)  -> foldM (\ve (e', p) -> patMatch (e', v) ve p) venv [(e1, p1), (e2, p2)]
--        (TupO es v, TupP ps)          -> foldM (\ve (e', p) -> patMatch (e', v) ve p) venv $ zip es ps
--        _                             -> fail ""
--        
-- 
-- letBind :: [(Id, Exp)] -> VEnv -> EvalM VEnv
-- letBind xes venv =
--   do rec venv' <- foldM (\acc (x, e) -> veInsert x (e, venv') acc
--                         ) venv xes
--      return venv'
-- 
-- veInsert :: Id -> (Exp, VEnv) -> VEnv -> EvalM VEnv
-- veInsert x (e, ve) (VEnv ve') =
--   do ref <- lift $ newIORef $ Cl e ve
--      return $ VEnv (Mp.insert x ref ve')
-- 
-- veLookup :: Id -> VEnv -> EvalM Whnf
-- veLookup x (VEnv venv) = 
--   do when (Mp.notMember x venv) (throwEvalError ("unbound var: " ++ show x))
--      cont <- lift $ readIORef ref
--      case cont of
--        Cl e venv' -> do w <- reduce e venv'
--                         lift $ writeIORef ref (Wh w)
--                         return w
--        Wh w       -> return w
--   where ref = venv Mp.! x
--         
-- intCheck :: Whnf -> EvalM Int
-- intCheck (IntO i) = return i
-- intCheck _        = throwEvalError "int is expected"
-- 
-- 
-- boolCheck :: Whnf -> EvalM Bool
-- boolCheck (BoolO b) = return b
-- boolCheck _         = throwEvalError "bool is expected"
-- 
-- funCheck :: Whnf -> EvalM (Id, Exp, VEnv)
-- funCheck (FunO x e venv) = return (x, e, venv)
-- funCheck _               = throwEvalError "fun is expected"
--   
-- throwEvalError :: String -> EvalM a
-- throwEvalError = throwLangError . Eval



-- not using IO monad --

evalMain :: Exp -> LangM () --Result
evalMain exp = flip evalStateT initRefs $ eval exp
  where initRefs = Refs Mp.empty

eval :: Exp -> EvalM () --Result
eval exp = {-topPrint =<<-} void $ reduce exp initvenv
  where initvenv = VEnv Mp.empty

topPrint :: Whnf -> EvalM ()
topPrint ob =
  case ob of
--    IntO i           -> return $ show i
--    BoolO b          -> return $ show b
--    FunO _ _ _       -> return "*function*"
--    NilO             -> return "[]"
--    ConsO e1 e2 venv -> pure (\x y z -> x++y++z)
--                        <*> (topPrint =<< reduce e1 venv)
--                        <*> return ":"
--                        <*> (topPrint =<< reduce e2 venv)
--    TupO es venv     -> pure (\x y z -> x++y++z)
--                        <*> return "(" 
--                        <*> (concat . intersperse ", ") <$> mapM ((topPrint =<<) . flip reduce venv) es
--                        <*> return ")"
--    UserO c es venv  -> do rs <- mapM (\e -> topPrint =<< reduce e venv) es
--                           return (c ++ concat (map (" "++) rs))
    IOO UnitE venv   -> return ()
--    IOO e venv       -> error "EvalLazy.hs - topPrint"
    IOWO UnitO       -> return ()
--    IOWO _           -> error "EvalLazy.hs - topPrint"


reduce :: Exp -> VEnv -> EvalM Whnf
reduce exp venv =
  case exp of
    UnitE        -> return $ UnitO
    LetE xes e2  -> reduce e2 =<< letBind xes venv
    VarE x       -> veLookup x venv
    AbsE x e     -> return $ FunO x e venv
    AppE e1 e2   -> do (x, e, venv') <- funCheck =<< reduce e1 venv
                       reduce e =<< veInsert x (e2, venv) venv'
    IntE i       -> return $ IntO i
    BoolE b      -> return $ BoolO b
    NilE         -> return $ NilO
    ConsE e1 e2  -> return $ ConsO e1 e2 venv
    TupE es      -> return $ TupO es venv
    CaseE e1 pes ->
      do let mmws = map (\(pat, e) -> do ve <- patMatch (e1, venv) venv pat -- [MaybeT EvalM Whnf]
                                         lift $ reduce e ve
                        ) pes
         mayw <- runMaybeT (msum mmws) -- gathering first mathing line
         case mayw of
           Just w  -> return w
           Nothing -> throwEvalError "EVAL:pattern match failer"
    IfE e1 e2 e3 -> 
      do b <- boolCheck =<< reduce e1 venv
         if b then reduce e2 venv else reduce e3 venv
    OpE o e1 e2 ->
      do i1 <- intCheck =<< reduce e1 venv
         i2 <- intCheck =<< reduce e2 venv
         when (o == DivC && i2 == 0) $ throwEvalError "division by zero"
         return $ case o of
           PlusC   -> IntO (i1 + i2)
           MinusC  -> IntO (i1 - i2)
           MulC    -> IntO (i1 * i2)
           DivC    -> IntO (i1 `div` i2)
           EqC     -> BoolO (i1 == i2)
           NeqC    -> BoolO (i1 /= i2)
           LtC     -> BoolO (i1 < i2)
           GtC     -> BoolO (i1 > i2)
           LeC     -> BoolO (i1 <= i2)
           GeC     -> BoolO (i1 >= i2)
    AssertE      -> throwEvalError "asserting"
    UserE c      -> flip reduce venv =<< constructorAbs c
    UserE' c es  -> return $ UserO c es venv
    IORetE e    -> return $ IOO e venv
    IOBindE m f ->
      do w <- reduce m venv
         FunO x ebody venv' <- reduce f venv
         case w of
           IOO  e venv'' -> reduce ebody =<< veInsert x (e, venv'') venv'
           IOWO w        -> reduce ebody =<< veInsertW x w venv'
    IOPutcE e   ->
      do IntO i <- reduce e venv
         liftIO $ putChar (intToDigit i)
         return $ IOWO UnitO
    IOGetcE     -> 
      do i <- digitToInt <$> liftIO getChar
         return $ IOWO (IntO i)

constructorAbs :: Constructor -> EvalM Exp -- eta transformation of constructor
constructorAbs c =
  do constMap <- lift getConstInfo
     let argn = length . snd $ constMap Mp.! c
     args <- sequence . take argn . repeat $ (("x."++) . show <$> lift freshGen)
     let e' = UserE' c (map VarE args)
     return $ foldr (\arg e -> AbsE arg e) e' args

-- recursively pattern-match, updating value-env
patMatch :: (Exp, VEnv) -> VEnv -> Pattern -> MaybeT EvalM VEnv
patMatch _       venv WildP    = return venv
patMatch ev      venv (VarP x) = lift $ veInsert x ev venv
patMatch (e, ve) venv pat =
  do w <- lift $ reduce e ve
     case (w, pat) of
       (UnitO, UnitP)                -> return venv
       (IntO i, IntP i')   | i == i' -> return venv
       (BoolO b, BoolP b') | b == b' -> return venv
       (NilO, NilP)                  -> return venv
       (ConsO e1 e2 v, ConsP p1 p2)  -> foldM (\ve (e', p) -> patMatch (e', v) ve p) venv [(e1, p1), (e2, p2)]
       (TupO es v, TupP ps)          -> foldM (\ve (e', p) -> patMatch (e', v) ve p) venv $ zip es ps
       (UserO c es v, UserP c' ps)
         | c == c'                   -> foldM (\ve (e', p) -> patMatch (e', v) ve p) venv $ zip es ps
         | otherwise                 -> fail ""
       _                             -> fail ""
       

letBind :: [(Id, Exp)] -> VEnv -> EvalM VEnv
letBind xes venv =
  do rec venv' <- foldM (\acc (x, e) -> veInsert x (e, venv') acc
                        ) venv xes
     return venv'

veInsert :: Id -> (Exp, VEnv) -> VEnv -> EvalM VEnv
veInsert x (e, ve) (VEnv ve') =
  do ref <- lift$freshGen
     newRef ref (e, ve)
     return $ VEnv (Mp.insert x ref ve')
     
veInsertW :: Id -> Whnf -> VEnv -> EvalM VEnv
veInsertW x w (VEnv ve') =
  do ref <- lift$freshGen
     newRefW ref w
     return $ VEnv (Mp.insert x ref ve')

veLookup :: Id -> VEnv -> EvalM Whnf
veLookup x (VEnv venv) = 
  do when (Mp.notMember x venv) (throwEvalError ("unbound var: " ++ show x))
     cont <- readRef ref
     case cont of
       Cl e venv' -> do w <- reduce e venv'
                        writeRef ref w
                        return w
       Wh w       -> return w
  where ref = venv Mp.! x
        
newRef :: Ref -> (Exp, VEnv) -> EvalM ()
newRef ref (e, ve) = modify $ Refs . Mp.insert ref (Cl e ve) . unRefs

newRefW :: Ref -> Whnf -> EvalM ()
newRefW ref w = modify $ Refs . Mp.insert ref (Wh w) . unRefs

readRef :: Ref -> EvalM Object
readRef ref = gets $ fromJust . Mp.lookup ref . unRefs

writeRef :: Ref -> Whnf -> EvalM ()
writeRef ref w = modify $ Refs . Mp.insert ref (Wh w) . unRefs
        
intCheck :: Whnf -> EvalM Int
intCheck (IntO i) = return i
intCheck _        = throwEvalError "int is expected"


boolCheck :: Whnf -> EvalM Bool
boolCheck (BoolO b) = return b
boolCheck _         = throwEvalError "bool is expected"

funCheck :: Whnf -> EvalM (Id, Exp, VEnv)
funCheck (FunO x e venv) = return (x, e, venv)
funCheck _               = throwEvalError "fun is expected"
  
throwEvalError :: String -> EvalM a
throwEvalError = lift . throwLangError . EvalEr
