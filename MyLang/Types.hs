{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, DoRec, MultiParamTypeClasses, UndecidableInstances #-}

module Types where

import qualified Data.Map as Mp
import Data.IORef
-- import Control.Monad.Trans.Free
-- import Control.Monad.Fix (MonadFix, mfix)
import Control.Monad.Identity
--import Control.Monad.Trans (lift, MonadTrans)
import Control.Monad.Error
import Control.Monad.State
import Control.Applicative
import Control.Monad.Identity (runIdentity)
import Text.ParserCombinators.Parsec (ParseError)

---------
-- AST --
---------

data Exp = UnitE
         | IntE Int
         | BoolE Bool 
         | NilE
         | ConsE Exp Exp
         | TupE [Exp]
         | VarE Id
         | OpE OpC Exp Exp
         | IfE Exp Exp Exp
         | AppE Exp Exp
         | AbsE Id Exp
         | LetE [(Id, Exp)] Exp
         | CaseE Exp [(Pattern, Exp)]
         | AssertE
         | UserE Constructor
         | UserE' Constructor [Exp]
         | IORetE Exp           -- primitive io
         | IOBindE Exp Exp      -- primitive io
         | IOPutcE Exp          -- primitive io
         | IOGetcE              -- primitive io
         deriving Show
                  
data Pattern = UnitP
             | IntP Int                  
             | BoolP Bool
             | NilP
             | ConsP Pattern Pattern
             | TupP [Pattern]               
             | VarP Id
             | WildP
             | UserP Constructor [Pattern]
             deriving Show

-- these operations are limited to Int Expression
data OpC = PlusC | MinusC | MulC | DivC
         | EqC | NeqC | LtC | GtC | LeC | GeC
         deriving (Show, Eq)
                  
opKind :: OpC -> Bool                  
opKind o =
  case o of
    PlusC  -> True
    MinusC -> True
    MulC   -> True
    DivC   -> True
    EqC    -> False
    NeqC   -> False
    LtC    -> False
    GtC    -> False
    LeC    -> False
    GeC    -> False
    
type Id = String

------------------
-- for evallazy --
------------------

-- weak head normal form
data Whnf = UnitO
          | IntO Int
          | BoolO Bool
          | NilO
          | ConsO Exp Exp VEnv
          | TupO [Exp] VEnv
          | FunO Id Exp VEnv
          | UserO Constructor [Exp] VEnv
          | IOO Exp VEnv        -- for unitIO
          | IOWO Whnf           -- Placeholder for IO-Primitive function
            
-- thunk or evaluated
data Object = Cl Exp VEnv
            | Wh Whnf

-- value environment for eval
--newtype VEnv = VEnv {unVEnv :: Mp.Map Id (IORef Object)}
-- monad of lazy evalation
--type EvalM = LangM


-- not using IOref version--
type EvalM = StateT Refs LangM
type Result = String

newtype Refs = Refs {unRefs :: Mp.Map Ref Object}
type Ref = Int

newtype VEnv = VEnv {unVEnv :: Mp.Map Id Ref}

-----------
-- Error --
-----------

data LangError = HogeEr
               | EvalEr String
               | ParseEr ParseError
               | TypeEr String deriving Show

instance Error LangError where
  strMsg _ = HogeEr

-----------
-- Monad --
-----------

type    LangM      = LangMT IO -- Identity
newtype LangMT m a = LangMT {unLangMT :: StateT CompInfo (ErrorT LangError m) a}

data CompInfo = CompInfo {fresh     :: Int, 
                          typeInfo  :: [TypeInfo], 
                          constInfo :: ConstructorInfo}
initCompInfo = CompInfo 0 [] Mp.empty

-- monad api --
--runLangM :: LangM a -> Either LangError a
--runLangM = runIdentity . runLangMT

runLangMT :: Monad m => LangMT m a -> m (Either LangError a)
runLangMT (LangMT ma) = runErrorT $ evalStateT ma initCompInfo

freshGen :: Monad m => LangMT m Int
freshGen = LangMT $ modify (\ci -> ci {fresh = fresh ci + 1}) >> gets fresh

putInfo :: Monad m => [TypeInfo] -> LangMT m ()
putInfo tis = LangMT $ modify (\ci -> ci {typeInfo = tis, constInfo = infoConvert tis})

getTypeInfo :: Monad m => LangMT m [TypeInfo]
getTypeInfo = LangMT $ gets typeInfo

getConstInfo :: Monad m => LangMT m ConstructorInfo
getConstInfo = LangMT $ gets constInfo

throwLangError :: Monad m => LangError -> LangMT m a
throwLangError e = LangMT $ throwError e

-- instances --
instance Functor m => Functor (LangMT m) where
  fmap f (LangMT ma) = LangMT (fmap f ma)
  
instance (Functor m, Monad m) => Applicative (LangMT m) where
  pure a = LangMT $ pure a
  (LangMT mab) <*> (LangMT ma) = LangMT $ mab <*> ma
  
instance Monad m => Monad (LangMT m) where
  return a          = LangMT $ return a
  (LangMT ma) >>= f = LangMT $ ma >>= unLangMT . f
  
instance MonadTrans LangMT where
  lift = LangMT . lift . lift
  
instance MonadState s m => MonadState s (LangMT m) where
  get = lift get
  put = lift . put
  
instance MonadFix m => MonadFix (LangMT m) where
  mfix f = LangMT $ mfix (\a -> unLangMT (f a))
  
instance MonadIO (LangMT IO) where
  liftIO iox = lift iox
    
---------------------------
-- type for typechecking --
---------------------------

data Type = UnitT 
          | IntT
          | BoolT
          | ListT Type
          | TupT [Type]
          | FunT Type Type
          | VarT TId
          | IOT Type
          | UserT Inductive [Type]
          | ScheT [TId] Type
          deriving (Show, Eq)

type TId = Int
type Inductive   = String
type Constructor = String

type TypeInfo = (Type, [(Constructor, [Type])])                 -- user-defined-type-info type using in parse
type ConstructorInfo = Mp.Map Constructor (Type, [Type])        -- user-defined-type-info type using in internal

-- example
{-
data Tree = Node Int Tree Tree
          | Leaf
("Tree", [("Node", [IntT, UserT "Tree", UserT "Tree"]), ("Leaf", [])])


data Tree {a} = Node a (Tree {a}) (Tree {a})
              | Leaf

("Tree", ["a"], [("Node", ["a", UserT "Tree", UserT "Tree"]), ("Leaf", [])])
("Tree", ["a"], [("Node", [VarT "a", UserT "Tree" ["a"], UserT "Tree" ["a"]]), ("Leaf", [])])


data List a = Cons a (List a)
            | Nil
("List", ["a"], [("Cons", [VarT "a", UserT "List" ["a"]]), ("Nil", [])])


initTypeInfos :: [TypeInfo]
initTypeInfos = [(UserT "Tree" [VarT 0], [("Node", [VarT 0, UserT "Tree" [VarT 0], UserT "Tree" [VarT 0]]),
                                          ("Leaf", [])])
                ]
-}

infoConvert :: [TypeInfo] -> ConstructorInfo
infoConvert tis = Mp.fromList 
                  $ concatMap (\(userTy, cs) -> map (\(c, argts) -> (c, (userTy, argts))) cs
                              ) tis

typesToFun :: (Type, [Type]) -> Type -- trans from representation of constructor to function Type
typesToFun (t, ts) = foldr (\t acc -> FunT t acc) t ts

type TypingM = StateT TContext LangM

data TContext = TContext {constraints :: Constraints,
                          tmap        :: TMap}
                
type TEnv = Mp.Map Id Type
type Constraints  = [(Type, Type)]
type TMap = Mp.Map TId Type
