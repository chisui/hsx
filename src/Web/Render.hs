module Web.Render where

import "base" Data.String ( IsString(..) )
import "base" Data.Semigroup
import "base" Data.Coerce

import "text" Data.Text ( Text )

import "this" Data.HKD


newtype RenderS = RenderS (Text -> Text)

runRenderS :: RenderS -> Text
runRenderS r = (coerce r :: Text -> Text) mempty

instance IsString RenderS where
    fromString s = RenderS (fromString s <>)

instance Semigroup RenderS where
    (<>) = coerce ((.) :: (Text -> Text) -> (Text -> Text) -> (Text -> Text))
    stimes = stimesMonoid
instance Monoid RenderS where
    mempty = coerce (id :: Text -> Text)


class Renderable a where
    render :: a -> RenderS

class Monad f => Renderable1 f where
    liftRender1 :: (a -> RenderS) -> f a -> RenderS

class RenderableH1 g where
    renderH1 :: Renderable1 f => g f -> RenderS

render1 :: (Renderable a, Renderable1 f) => f a -> RenderS
render1 = liftRender1 render

instance Renderable Text where
    render s = coerce (s <>)

instance Renderable RenderS where
    render = id

instance Renderable a => Renderable [a] where
    render = render1
instance Renderable1 [] where
    liftRender1 = foldMap


instance (Renderable a, Renderable1 f) => Renderable (Ap a f) where
    render = renderH1
instance Renderable a => RenderableH1 (Ap a) where
    renderH1 (Ap a) = render1 a

instance RenderableH1 g => RenderableH1 (HKL g) where
    renderH1 = (liftRender1 . liftRender1 . liftRender1) renderH1 . getHKL
