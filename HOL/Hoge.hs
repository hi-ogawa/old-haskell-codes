{-# LANGUAGE FlexibleContexts #-}

module Hoge where

import Control.Monad.Error

-- syntax --
data Term = UnivT Universe
          | VarT Id
          | ProdT Id Term Term
          | LambT Id Term Term
          | AppT Term Term

type Id = String

-- universe --
data Universe = Type'
              | Type
              | Prop
    
type Env = [(Id, Term)]

type HogeMonad a = Either String a

typeCheck :: Env -> Term -> HogeMonad Term
typeCheck env e =
  case e of
    UnivT Type' -> throw
    UnivT u     -> ifM (envCheck env) (return e) throw
    VarT x      -> ifM (envCheck env) (maybeToErr $ lookup x env) throw
    AppT e1 e2  -> do t1 <- typeCheck env e1
                      t2 <- typeCheck env e2
                      toWHNF t1
                      undefined
    LambT x e1 e2 -> do t2 <- typeCheck env e2 -- env' = env ++ [x : e1] ??
                        undefined
                        return $ ProdT x e1 t2
    -- ProdT x e1 e2 -> do 

envCheck :: Env -> HogeMonad Bool
envCheck = undefined

betaReduce = undefined

toWHNF :: Term -> HogeMonad Term
toWHNF e = undefined

isProdT = undefined  




ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM mb a1 a2 = do b <- mb
                  if b then a1 else a2

maybeToErr :: MonadError String m => Maybe a -> m a
maybeToErr (Just a) = return a
maybeToErr Nothing  = throwError $ strMsg "nothing"

throw :: MonadError String m => m a
throw = throwError $ strMsg "throw"


