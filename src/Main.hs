{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import "text" Data.Text ( Text )
import "text" Data.Text.IO qualified as Text

import "this" Web.Render
import "this" Web.Hsx
import "this" Web.HsxCtx


renderHsxCtx :: HsxCtx (HtmlNode HsxCtx) -> Text
renderHsxCtx = runRenderS . render

main :: IO ()
main = do
    Text.putStrLn . renderHsxCtx $ do
        pure $ ElementNode "div" [attr (pure "a") (pure "b")]
            [ "bla"
            , "blo"
            ]
