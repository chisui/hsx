module Web.Hsx where

import "base" Data.String ( IsString(..) )
import "text" Data.Text ( Text )

import "this" Web.Render
import "this" Data.HKD


data HtmlNode f
    = ElementNode (Ap Text f) (HKL Attr f) (HKL HtmlNode f)
    | TextNode (Ap Text f)

newtype Attr f = Attr (HKPair (Ap Text) (Ap Text) f)
attr :: Applicative f => f Text -> f Text -> f (Attr f)
attr k v = pure . Attr . HKPair $ (Ap k, Ap v)
instance RenderableH1 Attr where
    renderH1 (Attr (HKPair (k, v)))
        = " " <> render k <> "=\"" <> render v <> "\""

instance Applicative f => IsString (HtmlNode f) where
    fromString = TextNode . Ap . pure . fromString

instance Renderable1 f => Renderable (HtmlNode f) where
    render = renderH1
instance RenderableH1 HtmlNode where
    renderH1 (TextNode f) = renderH1 f
    renderH1 (ElementNode n a cs)
        = "<" <> renderH1 n <> renderH1 a <> ">"
        <> renderH1 cs
        <> "</" <> renderH1 n <>  ">"
