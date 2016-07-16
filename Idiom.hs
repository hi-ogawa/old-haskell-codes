import Data.Monoid
import Control.Applicative hiding (Const, getConst)
import Data.Traversable

newtype Const a b = Const {getConst :: a}

instance Functor (Const a) where
  fmap f = Const . getConst
  
instance Monoid a => Applicative (Const a) where
  pure x = Const mempty
  Const x <*> Const y = undefined

contents :: Traversable t => t a -> [a]
contents = getConst . traverse (\x -> Const [x])
