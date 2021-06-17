module Web.Hsx.HtmlNode where

import "base" Data.String ( IsString(..) )
import "base" Data.Functor.Const
import "base" Data.Coerce

import "text" Data.Text ( Text, pack )

import "this" Web.Render
import "this" Data.HKD


newtype Attr f = Attr (HKPair (Const Text) (Ap Text) f)

instance RenderableH1 Attr where
    renderH1 (Attr (HKPair (k, v)))
        = " " <> render k <> "=\"" <> render v <> "\""

attr :: Applicative f => Text -> f Text -> f (Attr f)
attr k v = pure (coerce (Const k, Ap v))


data HtmlNode f
    = ElementNode Text (HKL Attr f) (HKL HtmlNode f)
    | TextNode Text

toHtml :: (Show a, Applicative f) => a -> HKL HtmlNode f
toHtml = HKL . pure . pure . pure . TextNode . pack . show

instance Applicative f => IsString (HtmlNode f) where
    fromString = TextNode . fromString

instance Renderable1 f => Renderable (HtmlNode f) where
    render = renderH1
instance RenderableH1 HtmlNode where
    renderH1 (TextNode f) = render f
    renderH1 (ElementNode n a cs)
        = "<" <> render n <> renderH1 a <> ">"
        <> renderH1 cs
        <> "</" <> render n <> ">"
