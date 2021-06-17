{-# LANGUAGE QuasiQuotes #-}
module Main where

import "text" Data.Text.IO qualified as Text

import "this" Web.Render
import "this" Web.Hsx


renderHsxCtx :: Hsx -> IO ()
renderHsxCtx = Text.putStrLn . runRenderS . render

main :: IO ()
main = renderHsxCtx [hsx|
        <div>
            adsf ${child} bsdf ${"literal"} csdf ${toHtml 12}
            ${if True
                then "yes" 
                else "no"
            }
            ${multiple}
        </div>
    |]
  where
    child = [hsx|<a href="https://www.youtube.com/watch?v=dQw4w9WgXcQ">link</a>|] :: Hsx
    multiple = ["a", "b"] :: Hsx
