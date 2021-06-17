module Web.Hsx ( module Exp, type Hsx ) where

import "this" Web.Hsx.HtmlNode as Exp
import "this" Web.Hsx.HsxCtx as Exp
import "this" Web.Hsx.Quote as Exp

import "this" Data.HKD


type Hsx = HKL HtmlNode HsxCtx
