{-# LANGUAGE NoImplicitPrelude #-}
module Web.Hsx.Parser.Tokens
    ( parseTokens
    , token
      -- * Types
    , Token(..)
    , TagName, AttrName, AttrValue
    , Attr(..)
      -- * Rendering, text canonicalization
    , renderTokens
    , renderToken
    , renderAttrs
    , renderAttr
    , canonicalizeTokens
    ) where

import "base" Prelude hiding ( take, takeWhile )
import "base" Data.Char hiding ( isSpace )
import "base" Data.List ( unfoldr )
import "base" Data.String ( IsString(..) )
import "base" Data.Functor ( ($>) )
import "base" GHC.Generics
import "base" Control.Applicative
import "base" Control.Monad ( guard )

import "deepseq" Control.DeepSeq

import "attoparsec" Data.Attoparsec.Text

import "text" Data.Text ( Text )
import "text" Data.Text qualified as T
import "text" Data.Text.Lazy qualified as TL
import "text" Data.Text.Lazy.Builder ( Builder )
import "text" Data.Text.Lazy.Builder qualified as B


type TagName   = Text
type AttrName  = Text
type Expr      = Text
data AttrValue
    = AttrValue       !Text
    | AttrValueSplice !Text
  deriving (Show, Eq, Ord, Generic)

data Token
    = TagOpen      !TagName [Attr]
    | TagSelfClose !TagName [Attr]
    | TagClose     !TagName
    | ContentText  !Text
    | Comment      !Builder
    | Doctype      !Text
    | Splice       !Expr
  deriving (Show, Ord, Eq, Generic)

instance IsString Token where
    fromString = ContentText . fromString

endOfFileToken :: Token
endOfFileToken = ""

data Attr
    = Attr !AttrName !AttrValue
    | AttrSplice !Expr
  deriving (Show, Eq, Ord)

instance NFData Token where
    rnf (Comment b) = rnf $ B.toLazyText b
    rnf _           = ()

-- | Parse a single 'Token'.
token :: Parser Token
token = dataState -- Start in the data state.

-- | /§8.2.4.1/: Data state
dataState :: Parser Token
dataState = do
    content <- takeWhile $ not . (isC '<' |. isC '$')
    if not $ T.null content
      then pure $ ContentText content
      else choice
        [ "<" *> tagOpen
        , "$" *> (Splice <$> splice)
        ]

-- | /§8.2.4.6/: Tag open state
tagOpen :: Parser Token
tagOpen = choice
    [ "!" *> markupDeclOpen
    , "/" *> endTagOpen
    , "?" *> bogusComment mempty
    , tagNameOpen
    , pure "<"
    ]

splice :: Parser Text
splice = ("{" *> exprSplice) <|> varSplice
  where
    exprSplice = takeWhile (not . isC '}') <* "}"
    varSplice = liftA2 (<>) varHead varTail
    varHead = T.singleton <$> satisfy (isAlpha |. isC '_')
    varTail = takeWhile (isAlphaNum |. isC '_')

-- | /§8.2.4.7/: End tag open state
endTagOpen :: Parser Token
endTagOpen = tagNameClose

-- | Equivalent to @inClass "\x09\x0a\x0c "@
isWhitespace :: Char -> Bool
isWhitespace '\x09' = True
isWhitespace '\x0a' = True
isWhitespace '\x0c' = True
isWhitespace ' '    = True
isWhitespace _      = False

(|.) :: (Char -> Bool) -> (Char -> Bool) -> Char -> Bool
(|.) f g c = f c || g c
{-# INLINE (|.) #-}

isC :: Char -> Char -> Bool
isC = (==)
{-# INLINE isC #-}

-- | /§8.2.4.8/: Tag name state: the open case
--
-- deviation: no lower-casing, don't handle NULL characters
tagNameOpen :: Parser Token
tagNameOpen = do
    tag <- tagName'
    choice
        [ satisfy isWhitespace *> beforeAttrName tag []
        , "/" *> selfClosingStartTag tag []
        , ">" $> TagOpen tag []
        ]

-- | /§8.2.4.10/: Tag name state: close case
tagNameClose :: Parser Token
tagNameClose = do
    tag <- tagName'
    ">" $> TagClose tag

-- | /§8.2.4.10/: Tag name state: common code
--
-- deviation: no lower-casing, don't handle NULL characters
tagName' :: Parser Text
tagName' = do
    c <- peekChar'
    guard $ isAsciiUpper c || isAsciiLower c
    takeWhile $ not . (isWhitespace |. isC '/' |. isC '<' |. isC '>')

-- | /§8.2.4.40/: Self-closing start tag state
selfClosingStartTag :: TagName -> [Attr] -> Parser Token
selfClosingStartTag tag attrs = choice
    [ ">" $> TagSelfClose tag attrs
    , endOfInput $> endOfFileToken
    , beforeAttrName tag attrs
    ]

-- | /§8.2.4.32/: Before attribute name state
--
-- deviation: no lower-casing
beforeAttrName :: TagName -> [Attr] -> Parser Token
beforeAttrName tag attrs = do
    skipWhile isWhitespace
    choice
        [ "/" *> selfClosingStartTag tag attrs
        , ">" $> TagOpen tag attrs
        -- , "\"00' *> attrName tag attrs -- TODO: NULL
        , "$" *> attrsSplice tag attrs
        , attrName tag attrs
        ]

attrsSplice :: TagName -> [Attr] -> Parser Token
attrsSplice tag attrs = do
    expr <- splice
    beforeAttrName tag (AttrSplice expr : attrs)

-- | /§8.2.4.33/: Attribute name state
attrName :: TagName -> [Attr] -> Parser Token
attrName tag attrs = do
    name <- takeWhile $ not . (isWhitespace |. isC '/' |. isC '=' |. isC '>')
    choice
        [ endOfInput *> afterAttrName tag attrs name
        , "=" *> beforeAttrValue tag attrs name
        , try (do mc <- peekChar
                  case mc of
                    Just c | notNameChar c ->  afterAttrName tag attrs name
                    _ -> empty)
        ]
      -- <|> -- TODO: NULL
  where notNameChar = isWhitespace |. isC '/' |. isC '>'

-- | /§8.2.4.34/: After attribute name state
afterAttrName :: TagName -> [Attr] -> AttrName -> Parser Token
afterAttrName tag attrs name = do
    skipWhile isWhitespace
    choice
        [ "/" *> selfClosingStartTag tag attrs
        , "=" *> beforeAttrValue tag attrs name
        , ">" $> TagOpen tag (Attr name (AttrValue T.empty) : attrs)
        , endOfInput $> endOfFileToken
        , attrName tag (Attr name (AttrValue T.empty) : attrs)  -- not exactly sure this is right
        ]

-- | /§8.2.4.35/: Before attribute value state
beforeAttrValue :: TagName -> [Attr] -> AttrName -> Parser Token
beforeAttrValue tag attrs name = do
    skipWhile isWhitespace
    choice
        [ "\"" *> attrValueDQuoted tag attrs name
        , "'" *> attrValueSQuoted tag attrs name
        , "$" *> attrValueSplice tag attrs name
        , ">" $> TagOpen tag (Attr name (AttrValue T.empty) : attrs)
        , attrValueUnquoted tag attrs name
        ]

-- | /§8.2.4.37/: Attribute value (single-quoted) state
attrValueSplice :: TagName -> [Attr] -> AttrName -> Parser Token
attrValueSplice tag attrs name 
    = afterAttrValueQuoted tag attrs name . AttrValueSplice =<< splice

-- | /§8.2.4.36/: Attribute value (double-quoted) state
attrValueDQuoted :: TagName -> [Attr] -> AttrName -> Parser Token
attrValueDQuoted tag attrs name = do
    value <- takeWhile (/= '"')
    "\"" *> afterAttrValueQuoted tag attrs name (AttrValue value)

-- | /§8.2.4.37/: Attribute value (single-quoted) state
attrValueSQuoted :: TagName -> [Attr] -> AttrName -> Parser Token
attrValueSQuoted tag attrs name = do
    value <- takeWhile (/= '\'')
    "\'" *> afterAttrValueQuoted tag attrs name (AttrValue value)

-- | /§8.2.4.38/: Attribute value (unquoted) state
attrValueUnquoted :: TagName -> [Attr] -> AttrName -> Parser Token
attrValueUnquoted tag attrs name = do
    value <- takeTill $ isWhitespace |. isC '>'
    choice
        [ satisfy isWhitespace *> beforeAttrName tag attrs -- unsure: don't emit?
        , ">" $> TagOpen tag (Attr name (AttrValue value) : attrs)
        , endOfInput $> endOfFileToken
        ]

-- | /§8.2.4.39/: After attribute value (quoted) state
afterAttrValueQuoted :: TagName -> [Attr] -> AttrName -> AttrValue -> Parser Token
afterAttrValueQuoted tag attrs name value = choice
    [ satisfy isWhitespace *> beforeAttrName tag attrs'
    , "/" *> selfClosingStartTag tag attrs'
    , ">" $> TagOpen tag attrs'
    , endOfInput $> endOfFileToken
    ]
  where
    attrs' = Attr name value : attrs

-- | /§8.2.4.41/: Bogus comment state
bogusComment :: Builder -> Parser Token
bogusComment content = choice
    [ ">" $> Comment content
    , endOfInput $> Comment content
    , "\x00" *> bogusComment (content <> "\xfffd")
    , anyChar >>= \c -> bogusComment (content <> B.singleton c)
    ]

-- | /§8.2.4.42/: Markup declaration open state
markupDeclOpen :: Parser Token
markupDeclOpen = choice
    [ try comment_
    , try docType
    , bogusComment mempty
    ]
  where
    comment_ = "-" *> "-" *> commentStart
    docType = do
        -- switching this to asciiCI slowed things down by a factor of two
        s <- take 7
        guard $ T.toLower s == "doctype"
        doctype

-- | /§8.2.4.43/: Comment start state
commentStart :: Parser Token
commentStart = choice
    [ "-" *> commentStartDash
    , ">" $> Comment mempty
    , comment mempty
    ]

-- | /§8.2.4.44/: Comment start dash state
commentStartDash :: Parser Token
commentStartDash = choice
    [ "-" *> commentEnd mempty
    , ">" $> Comment mempty
    , endOfInput $> Comment mempty
    , comment (B.singleton '-')
    ]

-- | /§8.2.4.45/: Comment state
comment :: Builder -> Parser Token
comment content0 = do
    content <- B.fromText <$> takeWhile (not . (isC '-' |. isC '\x00' |. isC '<'))
    let content' = content0 <> content
    choice
        [ "<" *> commentLessThan (content' <> "<")
        , "-" *> commentEndDash content'
        , "\x00" *> comment (content' <> "\xfffd")
        , endOfInput $> Comment content'
        ]

-- | /§8.2.46/: Comment less-than sign state
commentLessThan :: Builder -> Parser Token
commentLessThan content = choice
    [ "!" *> commentLessThanBang (content <> "!")
    , "<" *> commentLessThan (content <> "<")
    , comment content
    ]

-- | /§8.2.47/: Comment less-than sign bang state
commentLessThanBang :: Builder -> Parser Token
commentLessThanBang content = choice
    [ "-" *> commentLessThanBangDash content
    , comment content
    ]

-- | /§8.2.48/: Comment less-than sign bang dash state
commentLessThanBangDash :: Builder -> Parser Token
commentLessThanBangDash content = choice
    [ "-" *> commentLessThanBangDashDash content
    , commentEndDash content
    ]

-- | /§8.2.49/: Comment less-than sign bang dash dash state
commentLessThanBangDashDash :: Builder -> Parser Token
commentLessThanBangDashDash content = choice
    [ ">" *> comment content
    , endOfInput *> comment content
    , commentEnd content
    ]

-- | /§8.2.4.50/: Comment end dash state
commentEndDash :: Builder -> Parser Token
commentEndDash content = choice
    [ "-" *> commentEnd content
    , endOfInput $> Comment content
    , comment (content <> "-")
    ]

-- | /§8.2.4.51/: Comment end state
commentEnd :: Builder -> Parser Token
commentEnd content = choice
    [ ">" $> Comment content
    , "!" *> commentEndBang content
    , "-" *> commentEnd (content <> "-")
    , endOfInput $> Comment content
    , comment (content <> "--")
    ]

-- | /§8.2.4.52/: Comment end bang state
commentEndBang :: Builder -> Parser Token
commentEndBang content = choice
    [ "-" *> commentEndDash (content <> "--!")
    , ">" $> Comment content
    , endOfInput $> Comment content
    , comment (content <> "--!")
    ]

-- | /§8.2.4.53/: DOCTYPE state
-- FIXME
doctype :: Parser Token
doctype = do
    content <- takeTill (=='>')
    _ <- ">"
    pure $ Doctype content

-- | Parse a lazy list of tokens from strict 'Text'.
parseTokens :: Text -> [Token]
parseTokens = unfoldr f
  where
    f :: Text -> Maybe (Token, Text)
    f t
      | T.null t = Nothing
      | otherwise =
        case parse token t of
            Done rest tok -> Just (tok, rest)
            Partial cont  ->
                case cont mempty of
                  Done rest tok -> Just (tok, rest)
                  _             -> Nothing
            _             -> Nothing

-- | See 'renderToken'.
renderTokens :: [Token] -> TL.Text
renderTokens = mconcat . fmap renderToken

-- | (Somewhat) canonical string representation of 'Token'.
renderToken :: Token -> TL.Text
renderToken = TL.fromStrict . mconcat . \case
    TagOpen n []         -> ["<", n, ">"]
    TagOpen n attrs      -> ["<", n, " ", renderAttrs attrs, ">"]
    TagSelfClose n attrs -> ["<", n, " ", renderAttrs attrs, " />"]
    TagClose n           -> ["</", n, ">"]
    ContentText t        -> [t]
    Comment builder      -> ["<!--", TL.toStrict $ B.toLazyText builder, "-->"]
    Doctype t            -> ["<!DOCTYPE", t, ">"]
    Splice t             -> ["${", t, "}"]

-- | See 'renderAttr'.
renderAttrs :: [Attr] -> Text
renderAttrs = T.unwords . fmap renderAttr . reverse

-- | Does not escape quotation in attribute values!
renderAttr :: Attr -> Text
renderAttr (Attr k v) = mconcat [k, "=", renderAttrValue v]
renderAttr (AttrSplice e) = mconcat ["${", e, "}"]

renderAttrValue :: AttrValue -> Text
renderAttrValue (AttrValue v) = mconcat ["\"", v, "\""]
renderAttrValue (AttrValueSplice e) = mconcat ["${", e, "}"]

-- | Meld neighoring 'ContentChar' and 'ContentText' constructors together and drops empty text
-- elements.
canonicalizeTokens :: [Token] -> [Token]
canonicalizeTokens = filter (/= ContentText "") . meldTextTokens

meldTextTokens :: [Token] -> [Token]
meldTextTokens = \case
      (ContentText t : ContentText t' : ts) -> meldTextTokens $ ContentText (t <> t') : ts
      (t : ts) -> t : meldTextTokens ts
      [] -> []
