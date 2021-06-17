{-# LANGUAGE TemplateHaskell #-}
module Web.Hsx.Quote ( hsx ) where

import "base" Control.Monad
import "containers" Data.Tree

import "text" Data.Text ( Text )
import "text" Data.Text qualified as Text
import "template-haskell" Language.Haskell.TH
import "template-haskell" Language.Haskell.TH.Syntax
import "template-haskell" Language.Haskell.TH.Quote

import "haskell-src-meta" Language.Haskell.Meta.Parse ( parseExp )

import "this" Data.HKD
import "this" Web.Hsx.HtmlNode
import "this" Web.Hsx.Parser.Tokens qualified as P
import "this" Web.Hsx.Parser.Tree ( tokensToForest )


mapExp :: (a -> ExpQ) -> [a] -> ExpQ
mapExp _ []       = [| [] |]
mapExp f (a : as) = [| $(f a) : $(mapExp f as) |]

elemExp :: Text -> [P.Attr] -> [Tree P.Token] -> ExpQ
elemExp n _ cs = [| ElementNode $(lift n) (HKL (pure [])) $(forestToExp cs) |]

txtExp :: Text -> ExpQ
txtExp = appE [| TextNode |] . lift

spliceExp :: Text -> ExpQ
spliceExp = either reportErrorExp pure . parseExp . Text.unpack


forestToExp :: [Tree P.Token] -> ExpQ
forestToExp ts = appE [| HKL . fmap join . sequence |] $ mapExp treeToExp ts
  where
    treeToExp (Node (P.TagOpen n as) cs) = [| pure [pure $(elemExp n as cs)] |]
    treeToExp (Node (P.TagSelfClose n as) cs) = [| pure [pure $(elemExp n as cs)] |]
    treeToExp (Node (P.ContentText t) []) = [| pure [pure $(txtExp t)] |]
    treeToExp (Node (P.Splice e) []) = [| getHKL |] `appE` spliceExp e
    treeToExp t = error $ "unexpected token: " ++ show t

reportErrorExp :: String -> ExpQ
reportErrorExp s = reportError s >> [| undefined |]

hsx :: QuasiQuoter
hsx =  QuasiQuoter
    { quoteExp
        = either (reportErrorExp . show) forestToExp 
        . tokensToForest
        . P.parseTokens
        . Text.pack
    , quotePat  = error "can not use hsx in Pattern"
    , quoteType = error "can not use hsx in Type"
    , quoteDec  = error "can not use hsx in Declaration"
    }
