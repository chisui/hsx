module Web.Render where

import "base" Data.String ( IsString(..) )
import "base" Data.Semigroup
import "base" Data.Coerce
import "base" Data.Functor.Const

import "text" Data.Text ( Text )

import "this" Data.HKD


type EndoText = Text -> Text
newtype RenderS = RenderS EndoText

renderText :: Text -> RenderS
renderText s = coerce (s <>)

runRenderS :: RenderS -> Text
runRenderS r = (coerce r :: EndoText) mempty

instance IsString RenderS where
    fromString = renderText . fromString

instance Semigroup RenderS where
    (<>) = coerce ((.) :: EndoText -> EndoText -> EndoText)
    stimes = stimesMonoid
instance Monoid RenderS where
    mempty = coerce (id :: EndoText)


class Renderable a where
    render :: a -> RenderS

class Renderable1 f where
    liftRender1 :: (a -> RenderS) -> f a -> RenderS

class RenderableH1 g where
    renderH1 :: Renderable1 f => g f -> RenderS

render1 :: (Renderable a, Renderable1 f) => f a -> RenderS
render1 = liftRender1 render

instance Renderable Text where
    render = renderText

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

instance (RenderableH1 g, Renderable1 f) => Renderable (HKL g f) where
    render = renderH1
instance RenderableH1 g => RenderableH1 (HKL g) where
    renderH1 = (liftRender1 . liftRender1 . liftRender1) renderH1 . getHKL

instance Renderable a => Renderable (Const a b) where
    render c = render (coerce c :: a)
instance Renderable a => RenderableH1 (Const a) where
    renderH1 = render
instance Renderable a => Renderable1 (Const a) where
    liftRender1 _ = render
