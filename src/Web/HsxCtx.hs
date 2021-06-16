module Web.HsxCtx where

import "base" Data.String ( IsString(..) )
import "base" Control.Applicative ( liftA2 )

import "free" Control.Monad.Free

import "this" Web.Render


data HsxF a where
    HFPure :: a -> HsxF a
    HFAp :: HsxF (a -> b) -> HsxF a -> HsxF b

instance Functor HsxF where
    fmap f (HFPure a) = HFPure (f a)
    fmap g (HFAp f a) = HFPure (.) <*> HFPure g <*> f <*> a

instance Applicative HsxF where
    pure = HFPure
    HFPure f <*> HFPure a = HFPure (f a)
    f <*> a = HFAp f a

newtype HsxCtx a = HsxCtx { unHsxCtx :: Free HsxF a }
  deriving newtype (Functor, Applicative, Monad)

instance Semigroup a => Semigroup (HsxCtx a) where
    (<>) = liftA2 (<>)
instance Monoid a => Monoid (HsxCtx a) where
    mempty = pure mempty

instance IsString a => IsString (HsxCtx a) where
    fromString = pure . fromString

instance Renderable a => Renderable (HsxCtx a) where
    render = render1

instance Renderable1 HsxCtx where
    liftRender1 r = iter go . fmap r . unHsxCtx
      where
        go (HFPure a) = a
        go (HFAp _ _) = "<!-- can not render ap -->"
