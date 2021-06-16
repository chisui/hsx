module Data.HKD where

import "base" GHC.Exts ( IsList(..) )
import "base" Data.String ( IsString(..) )
import "base" Data.Coerce
import "base" Control.Applicative


newtype HKPair f g h = HKPair { getHKPair :: (f h, g h) }
  deriving newtype (Eq, Ord, Show, Semigroup, Monoid)


newtype Ap a f = Ap { getAp :: f a }
  deriving newtype (Eq, Ord, Show)

instance Alternative f => Semigroup (Ap a f) where
    (<>) = coerce ((<|>) :: f a -> f a ->  f a)
instance Alternative f => Monoid (Ap a f) where
    mempty = coerce (empty :: f a)

instance (Applicative f, IsString a) => IsString (Ap a f) where
    fromString = coerce . pure @f . fromString @a


newtype HKL g f = HKL { getHKL :: f [f (g f)] }

instance Applicative f => IsList (HKL g f) where
    type Item (HKL g f) = f (g f)
    fromList = HKL . pure
    toList = error "can not turn HKL into List!"

instance Applicative f => Semigroup (HKL g f) where
    HKL a <> HKL b = HKL $ liftA2 (<>) a b
instance Applicative f => Monoid (HKL g f) where
    mempty = HKL $ pure []
