module Web.Hsx.Parser.Tree
    ( -- * Constructing forests
      tokensToForest
    , ParseTokenForestError(..), PStack(..)
    , nonClosing
      -- * Deconstructing forests
    , tokensFromForest
    , tokensFromTree
    ) where

import "text" Data.Text ( Text )
import "containers" Data.Tree
import "this" Web.Hsx.Parser.Tokens


tokensToForest :: [Token] -> Either ParseTokenForestError (Forest Token)
tokensToForest = f (PStack [] [])
  where
    f (PStack ss []) [] = Right (reverse ss)
    f pstack []         = Left $ ParseTokenForestErrorBracketMismatch pstack Nothing
    f pstack (t : ts)   = case t of
        TagOpen n _     -> if n `elem` nonClosing
                             then f (pushFlatSibling t pstack) ts
                             else f (pushParent t pstack) ts
        TagSelfClose {} -> f (pushFlatSibling t pstack) ts
        TagClose n      -> (`f` ts) =<< popParent n pstack
        ContentText _   -> f (pushFlatSibling t pstack) ts
        Comment _       -> f (pushFlatSibling t pstack) ts
        Doctype _       -> f (pushFlatSibling t pstack) ts
        Splice _        -> f (pushFlatSibling t pstack) ts

nonClosing :: [Text]
nonClosing = ["br", "hr", "img"]

data ParseTokenForestError =
    ParseTokenForestErrorBracketMismatch PStack (Maybe Token)
  deriving (Eq, Show)

data PStack = PStack
    { _pstackToplevelSiblings :: Forest Token
    , _pstackParents          :: [(Token, Forest Token)]
    }
  deriving (Eq, Show)

pushParent :: Token -> PStack -> PStack
pushParent t (PStack ss ps) = PStack [] ((t, ss) : ps)

popParent :: TagName -> PStack -> Either ParseTokenForestError PStack
popParent n (PStack ss ((p@(TagOpen n' _), ss') : ps))
    | n == n' = Right $ PStack (Node p (reverse ss) : ss') ps
popParent n pstack
    = Left $ ParseTokenForestErrorBracketMismatch pstack (Just $ TagClose n)

pushFlatSibling :: Token -> PStack -> PStack
pushFlatSibling t (PStack ss ps) = PStack (Node t [] : ss) ps


tokensFromForest :: Forest Token -> [Token]
tokensFromForest = mconcat . fmap tokensFromTree

tokensFromTree :: Tree Token -> [Token]
tokensFromTree (Node o@(TagOpen n _) ts) | n `notElem` nonClosing
    = [o] <> tokensFromForest ts <> [TagClose n]
tokensFromTree (Node t [])
    = [t]
tokensFromTree _
    = error "renderTokenTree: leaf node with children."
