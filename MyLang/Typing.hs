{-# LANGUAGE TupleSections #-}

module Typing where

import Types
import Control.Monad.State
import Control.Applicative (pure, (<*>), (<$>))
import qualified Data.Map as Mp
import Data.List (nub)
import Control.Arrow ((***), (&&&))
import Data.Maybe (fromJust)
import Control.Monad.Writer

typeCheckMain :: Exp -> LangM Exp
typeCheckMain exp = evalStateT (typeCheck exp) initContext
  where initContext = TContext [] Mp.empty

typeCheck :: Exp -> TypingM Exp
typeCheck exp = 
  do topType <- gatherC exp initTEnv
     tymp <- unify =<< getC
     if topType == IOT UnitT 
       then return exp 
       else throwTypeError $ "top level type must be `IO ()`"
  where initTEnv = Mp.empty
  
-- gathering type constraint --
gatherC :: Exp -> TEnv -> TypingM Type
gatherC exp tenv =
  case exp of
    UnitE        -> return UnitT
    IntE i       -> return IntT
    BoolE b      -> return BoolT
    NilE         -> ListT <$> freshTVar
    ConsE e1 e2  -> do t1 <- gatherC e1 tenv
                       t2 <- gatherC e2 tenv
                       addC (ListT t1, t2)
                       return (ListT t1)
    TupE es      -> do ts <- mapM (flip gatherC tenv) es
                       return $ TupT ts
    VarE x       -> maybe (throwTypeError ("not found: " ++ x)) scheCheck $ Mp.lookup x tenv
    OpE o e1 e2  -> do t1 <- gatherC e1 tenv
                       t2 <- gatherC e2 tenv
                       addC (IntT, t1); addC (IntT, t2)
                       return $ if opKind o then IntT else BoolT
    IfE e1 e2 e3 -> do t1 <- gatherC e1 tenv
                       t2 <- gatherC e2 tenv
                       t3 <- gatherC e3 tenv
                       addC (BoolT, t1); addC (t2, t3)
                       return t2
    AppE e1 e2   -> do t1 <- gatherC e1 tenv
                       t2 <- gatherC e2 tenv
                       rett <- freshTVar
                       addC (t1, FunT t2 rett)
                       return rett
    AbsE arg e   -> do argt <- freshTVar
                       t <- gatherC e (Mp.insert arg argt tenv)
                       return $ FunT argt t
    LetE binds e -> gatherLet binds e tenv
    CaseE e pes  -> gatherCase e pes tenv
    AssertE      -> freshTVar
    UserE c      -> getConstructorType c
    IORetE e      -> IOT <$> gatherC e tenv
    IOBindE e1 e2 -> do t1 <- gatherC e1 tenv
                        t2 <- gatherC e2 tenv
                        ta <- freshTVar
                        tmb <- freshTVar
                        addC (t1, IOT ta); addC (t2, FunT ta tmb)
                        return tmb
    IOPutcE e     -> do t <- gatherC e tenv
                        addC (t, IntT)
                        return UnitT
    IOGetcE       -> return $ IOT IntT

gatherLet :: [(Id, Exp)] -> Exp -> TEnv -> TypingM Type
gatherLet binds ebody tenv =
  do preCon <- getC -- preserve pre constraints
     putC []        -- reset constraints
     newts <- mapM (const freshTVar) xs
     let tenv' = mpInserts (zip xs newts) tenv
     ets <- mapM (flip gatherC tenv') es
     mapM addC $ zip newts ets
     -- let polymorphism --
     -- before gathering letbody-expression, must do unifing constraints for now.
     -- and updating tenv' by tmap.
     -- and type-variables that haven't unified in newts is tranformed to type-scheme.
     tmap <- unify =<< getC
     let tenv'' = Mp.map (fst . flip induceType tmap) tenv'
         ts = map (\x -> (\(t, tids) -> ScheT tids t) $ induceType (tenv'' Mp.! x) tmap) xs -- schelize binding variables(xs)
     addCs preCon -- restore pre constraints
     gatherC ebody (mpInserts (zip xs ts) tenv'') -- type-schema must not be in constraints??
  where xs = map fst binds
        es = map snd binds
        
scheCheck :: Type -> TypingM Type        
scheCheck (ScheT ids t) = do mp <- mapM (\i -> (i,) <$> freshTVar) ids
                             return $ subst t mp
  where subst t mp =
          case t of
            VarT i              -> fromJust (lookup i mp)
            UnitT               -> UnitT
            IntT                -> IntT
            BoolT               -> BoolT
            ListT t1            -> ListT (subst t1 mp)
            TupT ts             -> TupT $ map (flip subst mp) ts
            FunT t1 t2          -> FunT (subst t1 mp) (subst t2 mp)
            UserT ind ts        -> UserT ind $ map (flip subst mp) ts
            ScheT ids t         -> error "high-rank poly??"
scheCheck t             = return t

gatherCase :: Exp -> [(Pattern, Exp)] -> TEnv -> TypingM Type        
gatherCase e pes tenv =
  do et     <- gatherC e tenv
     (t:ts) <- mapM (\pe -> gatherOnePat et pe tenv) pes
     addCs $ map (t,) ts
     return t
        
gatherOnePat :: Type -> (Pattern, Exp) -> TEnv -> TypingM Type
gatherOnePat et (p, ebody) tenv =
  do (pt, bs) <- getPatBinds p
     addC (et, pt)
     gatherC ebody $ mpInserts bs tenv

-- return a tuple that type of pattern and new bindings  
getPatBinds :: Pattern -> TypingM (Type, [(Id, Type)]) 
getPatBinds p =
  case p of
    UnitP       -> return (UnitT, [])
    IntP  _     -> return (IntT, [])
    BoolP _     -> return (BoolT, [])
    NilP        -> do t <- freshTVar
                      return (ListT t, [])
    ConsP p1 p2 -> do (t1, bs1) <- getPatBinds p1; (t2, bs2) <- getPatBinds p2
                      addC (ListT t1, t2)
                      when (overlap (map fst $ bs1 ++ bs2)) $ throwTypeError ("overlapping pattern-variable: " ++ show p)
                      return (ListT t1, bs1 ++ bs2)
    TupP  ps    -> do tbss <- mapM getPatBinds ps
                      let ts  = map fst tbss
                          bss = map snd tbss
                      when (overlap (map fst $ concat bss)) $ throwTypeError ("overlapping pattern-variable: " ++ show p)
                      return (TupT ts, concat bss)
    VarP  x     -> do t <- freshTVar
                      return (t, [(x, t)])
    WildP       -> do t <- freshTVar
                      return (t, [])
    UserP c ps  -> do tbss <- mapM getPatBinds ps
                      let ts  = map fst tbss
                          bss = map snd tbss
                      (t, ts') <- getConstructorFields c
                      when (length ps /= length ts) $ throwTypeError ("number of constructor's argument wrong: "++ c)
                      addCs $ zip ts ts'
                      when (overlap (map fst $ concat bss)) $ throwTypeError ("overlapping pattern-variable: " ++ show p)
                      return (t, concat bss)
  where overlap ls = length (nub ls) /= length ls
        
-- type constraint unification --
unify :: Constraints -> TypingM TMap
unify []  = return Mp.empty
unify (tt:con) =
  case tt of
    (ListT t, ListT t')         -> unify $ (t, t'):con
    (TupT ts, TupT ts')
      | length ts == length ts' -> unify $ zip ts ts' ++ con
      | otherwise               -> throwTypeError $ "unify error: tuple length differ"
    (FunT t1 t2, FunT t1' t2')  -> unify $ (t1, t1'):(t2, t2'):con
    (UserT ind ts, UserT ind' ts')
      | ind == ind' && length ts == length ts' -> unify $ zip ts ts' ++ con
      | otherwise                              -> throwTypeError $ "unify error: " ++ ind ++ " vs "++ ind'
    (ScheT _ _, _)              -> error "type-schema must not be unified"
    (_, ScheT _ _)              -> error "type-schema must not be unified"
    (VarT i, VarT i') | i == i' -> unify con
    (VarT i, t)
      | checkRec i t            -> throwTypeError $ "unify error: construct infinite type"
      | otherwise               -> return . Mp.insert i t =<< unify (map (substType i t *** substType i t) con)
    (t, VarT i)
      | checkRec i t            -> throwTypeError $ "unify error: construct infinite type"
      | otherwise               -> return . Mp.insert i t =<< unify (map (substType i t *** substType i t) con)
    (t, t') | t == t'           -> unify con
            | otherwise         -> throwTypeError $ "unify error: " ++ show tt

checkRec :: TId -> Type -> Bool
checkRec i t =
  case t of
    IntT       -> False
    BoolT      -> False
    UserT _ ts -> or $ map (checkRec i) ts
    ListT t1   -> checkRec i t1
    TupT ts    -> or $ map (checkRec i) ts
    FunT t1 t2 -> checkRec i t1 || checkRec i t2
    VarT i' | i == i'   -> True
            | otherwise -> False
    ScheT _ _  -> error "type-schema must not be unified"

substType :: TId -> Type -> Type -> Type
substType i tsub t =
  case t of
    UnitT        -> UnitT
    IntT         -> IntT
    BoolT        -> BoolT
    UserT ind ts -> UserT ind $ map (substType i tsub) ts
    ListT t1     -> ListT $ substType i tsub t1
    TupT ts      -> TupT $ map (substType i tsub) ts
    FunT t1 t2   -> FunT (substType i tsub t1) (substType i tsub t2)
    VarT i' | i == i'   -> tsub
            | otherwise -> t


induceType :: Type -> TMap -> (Type, [TId])
induceType t mp = runWriter $ induce' t mp []
  where induce' :: Type -> TMap -> [TId] -> Writer [TId] Type
        induce' t mp sched =
          case t of
            UnitT        -> return UnitT
            IntT         -> return IntT
            BoolT        -> return BoolT
            UserT ind ts -> UserT ind <$> mapM (\t -> induce' t mp sched) ts
            ListT t1     -> ListT <$> induce' t1 mp sched
            TupT ts      -> TupT <$> mapM (\t -> induce' t mp sched) ts
            FunT t1 t2   -> FunT <$> induce' t1 mp sched <*> induce' t2 mp sched
            VarT i | Mp.member i mp -> induce' (mp Mp.! i) mp sched
                   | elem i sched   -> return (VarT i)
                   | otherwise      -> tell [i] >> return (VarT i)
            ScheT ids t  -> ScheT ids <$> induce' t mp (sched++ids)

-- API --
getConstructorType :: Constructor -> TypingM Type    
getConstructorType c =
  do constMap <- lift getConstInfo
     maybe (throwTypeError ("not found constructor: " ++ c)) (\x -> return . typesToFun =<< typeTVarUpdate x) $ Mp.lookup c constMap

 -- pattern type constraint
getConstructorFields :: Constructor -> TypingM (Type, [Type]) -- for typecheking (constructor's argument and returning type)
getConstructorFields c =
  do constMap <- lift getConstInfo
     maybe (throwTypeError ("not found constructor: " ++ c)) typeTVarUpdate $ Mp.lookup c constMap

mpInserts :: Ord k => [(k, e)] -> Mp.Map k e -> Mp.Map k e
mpInserts kes mp = foldl (\acc (k, e) -> Mp.insert k e acc) mp kes

addCs :: [(Type, Type)] -> TypingM ()
addCs tts = mapM_ addC tts

addC :: (Type, Type) -> TypingM ()
addC tt = modify (\TContext{constraints = c, tmap = m} -> TContext (tt:c) m)

getC :: TypingM Constraints
getC = constraints <$> get

putC :: Constraints -> TypingM ()
putC con = modify (\TContext{constraints = _, tmap = m} -> TContext con m)

freshTVar :: TypingM Type
freshTVar = lift $ VarT <$> freshGen

typeTVarUpdate :: (Type, [Type]) -> TypingM (Type, [Type])
typeTVarUpdate (ut@(UserT ind ts), ts') = do mp <- mapM (\t -> (t,) <$> freshTVar) ts
                                             return (subst ut mp, map (flip subst mp) ts')
  where subst :: Type -> [(Type, Type)] -> Type
        subst t mp = 
          case t of
            VarT i              -> fromJust (lookup t mp)
            UnitT               -> UnitT
            IntT                -> IntT
            BoolT               -> BoolT
            ListT t1            -> ListT (subst t1 mp)
            TupT ts             -> TupT $ map (flip subst mp) ts
            FunT t1 t2          -> FunT (subst t1 mp) (subst t2 mp)
            UserT ind ts        -> UserT ind $ map (flip subst mp) ts
            

throwTypeError :: String -> TypingM a
throwTypeError str = lift $ throwLangError (TypeEr str)