{-# LANGUAGE OverloadedStrings #-}

-- | PubList.hs
--
-- Renders hh.bib as the /publications.html page.
--
-- The page groups entries exactly the way cv.tex does, and for the same
-- reason: a bare title tells a visitor nothing about whether they are looking
-- at a book, a chapter, or a two-page review.  Both renderers read the SAME
-- `keywords` tags out of hh.bib, so the CV and the website cannot drift:
--
--     book-authored  -> Books
--     book-edited    -> Edited volumes
--     review         -> Book reviews
--     thesis         -> hidden here (it belongs under Education on the CV)
--     untagged       -> Articles (@article) or Chapters (everything else)
--
-- Halvorson is dropped from bylines, again matching the CV: a sole-authored
-- work shows no byline at all, a joint work shows "with A and B".
--
-- NOTE.  The sanitizer, author and venue helpers below are deliberately a
-- self-contained copy of the ones in EquivBiblio.hs.  They should eventually
-- move to a shared module; they are duplicated for now so that a change to
-- this page cannot break the Equivalent Theories bibliography.

module PubList
  ( parseBibTeXFile
  , transformEntry
  , generateHtml
  , generateRecent
  ) where

import Text.BibTeX.Parse (file)
import Text.BibTeX.Entry (T(..))
import Text.Parsec (ParseError, eof, parse)
import Data.List (sortOn, intercalate, isPrefixOf, isInfixOf)
import Data.Char (isAlpha, isSpace, toLower)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (Down(..))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.String as R

------------------------------------------------------------------------
-- Parsing
------------------------------------------------------------------------

parseBibTeXFile :: FilePath -> IO (Either ParseError [T])
parseBibTeXFile filePath = do
    content <- readFile filePath
    return (parseBibTeX content)

parseBibTeX :: String -> Either ParseError [T]
parseBibTeX = parse (file <* eof) ""

-- | Remove single-letter protective braces ("{R}ob" -> "Rob") and flatten the
-- whitespace that .bib line-wrapping leaves inside a field.  Without the
-- second step, a wrapped title carries its newline and leading indentation
-- straight into the HTML.
transformEntry :: T -> T
transformEntry (Cons entryType identifier fields) =
    Cons entryType identifier (map clean fields)
  where
    clean (k, v) = (k, squashSpace (removeBrackets v))

removeBrackets :: String -> String
removeBrackets [] = []
removeBrackets ('{':x:'}':xs) = x : removeBrackets xs
removeBrackets (x:xs) = x : removeBrackets xs

-- | Collapse every run of whitespace to a single space, and trim the ends.
squashSpace :: String -> String
squashSpace = trim . go
  where
    go []                     = []
    go (c:cs)
      | isSpace c             = ' ' : go (dropWhile isSpace cs)
      | otherwise             = c   : go cs

------------------------------------------------------------------------
-- Field accessors
------------------------------------------------------------------------

getField :: String -> T -> Maybe String
getField k (Cons _ _ fs) = case lookup k fs of
    Just v | not (null (trim v)) -> Just (trim v)
    _                            -> Nothing

getField' :: String -> T -> String
getField' k e = fromMaybe "" (getField k e)

bibKey :: T -> String
bibKey (Cons _ k _) = k

-- | Entry type, lowercased: the bibtex parser preserves the case written in
-- the file, so @PhdThesis and @phdthesis must compare equal.
bibType :: T -> String
bibType (Cons t _ _) = map toLower t

entryYear :: T -> Int
entryYear e = case reads (getField' "year" e) of
    [(n, _)] -> n
    _        -> 0

-- | hh.bib uses `keywords` for two different jobs: the routing tags this
-- module and cv.tex rely on, and free-text subject terms on a few entries.
-- Test for membership in the comma-separated list, never for equality.
hasKeyword :: String -> T -> Bool
hasKeyword kw e =
    kw `elem` map (map toLower . trim) (splitOn ',' (getField' "keywords" e))

splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
    (chunk, [])       -> [chunk]
    (chunk, _:rest)   -> chunk : splitOn c rest

------------------------------------------------------------------------
-- Sections
------------------------------------------------------------------------

data Kind = Book | Edited | Article | Chapter | Review | Thesis
  deriving (Eq)

kindOf :: T -> Kind
kindOf e
  | hasKeyword "thesis"        e   = Thesis
  | bibType e == "phdthesis"       = Thesis
  | hasKeyword "review"        e   = Review
  | hasKeyword "book-authored" e   = Book
  | hasKeyword "book-edited"   e   = Edited
  | bibType e == "article"         = Article
  | otherwise                      = Chapter

-- | Heading, anchor id, and the kind it collects.  Order is the order the
-- page renders in; Thesis is absent on purpose.
sections :: [(String, String, Kind)]
sections =
  [ ("Books",           "books",    Book)
  , ("Edited volumes",  "edited",   Edited)
  , ("Articles",        "articles", Article)
  , ("Chapters",        "chapters", Chapter)
  , ("Book reviews",    "reviews",  Review)
  ]

------------------------------------------------------------------------
-- LaTeX -> plain text
------------------------------------------------------------------------

sanitize :: String -> String
sanitize []                              = []
sanitize ('{' : xs)                      = sanitize xs
sanitize ('}' : xs)                      = sanitize xs
sanitize ('-':'-':'-': xs)               = '\8212' : sanitize xs  -- em-dash
sanitize ('-':'-'    : xs)               = '\8211' : sanitize xs  -- en-dash
sanitize ('`':'`'    : xs)               = '\8220' : sanitize xs  -- ``  -> "
sanitize ('\'':'\''  : xs)               = '\8221' : sanitize xs  -- ''  -> "
sanitize ('`'        : xs)               = '\8216' : sanitize xs  -- `   -> '
sanitize ('\\':'\'' : c : xs)            = withAccent acuteMap  c xs
sanitize ('\\':'`'  : c : xs)            = withAccent graveMap  c xs
sanitize ('\\':'"'  : c : xs)            = withAccent umlautMap c xs
sanitize ('\\':'^'  : c : xs)            = withAccent circumMap c xs
sanitize ('\\':'~'  : c : xs)            = withAccent tildeMap  c xs
sanitize ('\\':'c':'{': c :'}': xs)      =
  fromMaybe c (lookup c cedillaMap) : sanitize xs
sanitize ('\\':'&'  : xs)                = '&' : sanitize xs
sanitize ('\\': c : xs)
  | isAlpha c  = sanitize (dropWhile isAlpha xs)
  | otherwise  = sanitize xs
sanitize (x : xs)                        = x : sanitize xs

withAccent :: [(Char,Char)] -> Char -> String -> String
withAccent m c xs = fromMaybe c (lookup c m) : sanitize xs

acuteMap, graveMap, umlautMap, circumMap, tildeMap, cedillaMap :: [(Char,Char)]
acuteMap  = [('a','á'),('e','é'),('i','í'),('o','ó'),('u','ú'),
             ('A','Á'),('E','É'),('I','Í'),('O','Ó'),('U','Ú'),
             ('y','ý'),('Y','Ý')]
graveMap  = [('a','à'),('e','è'),('i','ì'),('o','ò'),('u','ù'),
             ('A','À'),('E','È'),('I','Ì'),('O','Ò'),('U','Ù')]
umlautMap = [('a','ä'),('e','ë'),('i','ï'),('o','ö'),('u','ü'),
             ('A','Ä'),('E','Ë'),('I','Ï'),('O','Ö'),('U','Ü')]
circumMap = [('a','â'),('e','ê'),('i','î'),('o','ô'),('u','û'),
             ('A','Â'),('E','Ê'),('I','Î'),('O','Ô'),('U','Û')]
tildeMap  = [('a','ã'),('n','ñ'),('o','õ'),('A','Ã'),('N','Ñ'),('O','Õ')]
cedillaMap = [('c','ç'),('C','Ç')]

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

------------------------------------------------------------------------
-- Bylines
------------------------------------------------------------------------

splitByAnd :: String -> [String]
splitByAnd "" = [""]
splitByAnd s  = go s
  where
    sep = " and "
    n   = length sep
    go "" = [""]
    go h
      | sep `isPrefixOf` h = "" : go (drop n h)
      | otherwise           = let (w:ws) = go (tail h)
                              in  (head h : w) : ws

-- "Halvorson, Hans" -> "Hans Halvorson"
invertName :: String -> String
invertName s = case break (== ',') s of
  (sur, ',':rest) -> trim rest ++ " " ++ trim sur
  _                -> trim s

isHalvorson :: String -> Bool
isHalvorson n = "halvorson" `isInfixOf` map toLower n

joinNames :: [String] -> String
joinNames names = case names of
  []     -> ""
  [x]    -> x
  [x, y] -> x ++ " and " ++ y
  xs     -> intercalate ", " (init xs) ++ ", and " ++ last xs

-- | Co-authors (or, for an edited volume, co-editors) with Halvorson removed.
-- Empty when the work is his alone, which is the common case and the reason
-- the byline is omitted rather than repeated 69 times.
coNames :: T -> String
coNames e =
    let raw    = case getField "author" e of
                   Just a  -> a
                   Nothing -> getField' "editor" e
        names  = map (invertName . trim) (splitByAnd (sanitize raw))
        others = filter (\n -> not (null n) && not (isHalvorson n)) names
    in joinNames others

------------------------------------------------------------------------
-- Venue
------------------------------------------------------------------------

formatVenue :: T -> String
formatVenue e =
  let j   = sanitize <$> getField "journal"   e
      bt  = sanitize <$> getField "booktitle" e
      pb  = sanitize <$> getField "publisher" e
      vol = getField "volume" e
      pgs = sanitize <$> getField "pages"     e
  in case kindOf e of
    Book    -> fromMaybe "" pb
    Edited  -> fromMaybe "" pb
    Chapter ->
      "In " ++ fromMaybe "?" bt
            ++ maybe "" (\p -> " (" ++ p ++ ")") pb
    _       ->
      case j of
        Just jname -> jname ++ volPages vol pgs
        Nothing    -> fromMaybe (fromMaybe "" pb) bt
  where
    volPages v p =
      maybe "" (\x -> " " ++ x) v ++ maybe "" (\x -> ", " ++ x) p

------------------------------------------------------------------------
-- BibTeX snippet
------------------------------------------------------------------------

bibtexString :: T -> String
bibtexString (Cons et key fs) =
  "@" ++ et ++ "{" ++ key ++ ",\n"
  ++ concatMap showField (filter ((`notElem` internal) . fst) fs)
  ++ "}\n"
  where
    internal = ["abstract", "keywords", "pdf", "philpapers", "philsci", "arxiv"]
    showField (k, v) = "  " ++ k ++ " = {" ++ v ++ "},\n"

------------------------------------------------------------------------
-- Badges
------------------------------------------------------------------------

badge :: String -> String -> String -> H.Html
badge cls label url =
  H.a H.! A.href   (H.toValue url)
      H.! A.class_ (H.toValue cls)
      H.! A.target "_blank"
      H.! A.rel    "noopener noreferrer"
      $ H.toHtml label

-- | Free-to-read badges, listed after the DOI so that a reader who hits a
-- paywall sees the open copy immediately next to it.
--
-- `arxiv`, `philsci` and `philpapers` hold a bare identifier (or a full URL);
-- `url` holds whatever canonical link the entry already had, and is labelled
-- by its host rather than a useless generic "Link".
openBadges :: T -> [H.Html]
openBadges e = mapMaybe pick
  [ ("pdf",        const "PDF",  \v -> "/pdf/" ++ v ++ ".pdf")
  , ("arxiv",      const "arXiv", expand "https://arxiv.org/abs/" "")
  , ("philpapers", const "PhilPapers",
                   expand "https://philpapers.org/rec/" "")
  , ("philsci",    const "PhilSci",
                   expand "https://philsci-archive.pitt.edu/" "/")
  , ("url",        labelForHost, id)
  ]
  where
    pick (fld, mkLabel, mkUrl) =
      fmap (\v -> badge "bib-badge bib-badge-pre" (mkLabel v) (mkUrl v))
           (getField fld e)
    -- A bare id gets wrapped in the repository's URL shape; a value that is
    -- already a URL is left alone.
    expand before after v
      | "http" `isPrefixOf` v = v
      | otherwise             = before ++ v ++ after

-- | Name the destination of a bare `url` field, so the badge says where it
-- goes. Anything unrecognised falls back to "Link".
labelForHost :: String -> String
labelForHost u = case filter (\(h, _) -> h `isInfixOf` u) hosts of
    ((_, label) : _) -> label
    []               -> "Link"
  where
    hosts =
      [ ("philpapers.org",            "PhilPapers")
      , ("philsci-archive.pitt.edu",  "PhilSci")
      , ("arxiv.org",                 "arXiv")
      , ("plato.stanford.edu",        "SEP")
      , ("ndpr.nd.edu",               "NDPR")
      , ("hdl.handle.net",            "Full text")
      ]

bibtexToggle :: String -> H.Html
bibtexToggle key =
  H.a H.! A.class_ "bib-badge bib-badge-bib"
      H.! A.role   "button"
      H.! H.customAttribute "data-toggle"   "collapse"
      H.! H.customAttribute "aria-expanded" "false"
      H.! A.href   (H.toValue $ "#bib-" ++ key)
      $ "BibTeX"

------------------------------------------------------------------------
-- Entry renderer
------------------------------------------------------------------------

renderEntry :: Bool -> T -> H.Html
renderEntry showBib e = H.li H.! A.class_ "bib-entry" $ do

  -- Title, linked to the best available full text.
  let titleStr = sanitize (getField' "title" e)
  H.span H.! A.class_ "bib-title" $
    case titleHref e of
      Just url -> H.a H.! A.href (H.toValue url)
                      H.! A.target "_blank" $ H.toHtml titleStr
      Nothing  -> H.toHtml titleStr

  -- Venue and year.
  let venue = formatVenue e
  if null venue
    then return ()
    else do H.toHtml (". " :: String)
            H.em H.! A.class_ "bib-venue" $ H.toHtml venue
  H.toHtml (" (" :: String)
  H.span H.! A.class_ "bib-year" $ H.toHtml (show (entryYear e))
  H.toHtml (")." :: String)

  -- Byline, only when the work is not his alone.
  let others = coNames e
  if null others
    then return ()
    else H.span H.! A.class_ "bib-authors" $
           H.toHtml (" With " ++ others ++ ".")

  H.span H.! A.class_ "bib-links" $ do
    maybe (return ())
          (\d -> badge "bib-badge bib-badge-doi" "DOI"
                       ("https://doi.org/" ++ d))
          (getField "doi" e)
    sequence_ (openBadges e)
    if showBib then bibtexToggle (bibKey e) else return ()

  if showBib
    then H.div H.! A.class_ "collapse"
               H.! A.id (H.toValue $ "bib-" ++ bibKey e) $
           H.div H.! A.class_ "bib-bibtex-block" $
             H.pre $ H.code $ H.toHtml (bibtexString e)
    else return ()

-- | Prefer a copy the reader can actually open over one they cannot.
titleHref :: T -> Maybe String
titleHref e = case getField "pdf" e of
    Just p  -> Just ("/pdf/" ++ p ++ ".pdf")
    Nothing -> case getField "doi" e of
      Just d  -> Just ("https://doi.org/" ++ d)
      Nothing -> getField "url" e

------------------------------------------------------------------------
-- Page
------------------------------------------------------------------------

-- | The n newest publications, for the home page. Wrapped in .publications so
-- the same scoped styling applies as on the full list; no BibTeX toggles,
-- since this is a teaser rather than a reference.
generateRecent :: Int -> [T] -> String
generateRecent n entries = R.renderHtml $
  H.div H.! A.class_ "publications recent-pubs" $
    H.ul H.! A.class_ "bib-list" $
      mapM_ (renderEntry False) (take n ordered)
  where
    ordered = sortOn (Down . entryYear)
                     (filter ((/= Thesis) . kindOf) entries)

generateHtml :: Bool -> [T] -> String
generateHtml showBib entries = R.renderHtml $
  H.div H.! A.class_ "publications" $ do

    H.p H.! A.class_ "bib-intro" $ do
      H.toHtml ("Grouped the same way as the " :: String)
      H.a H.! A.href "/cv.pdf" $ H.toHtml ("CV" :: String)
      H.toHtml (". Where a paper is behind a paywall, any freely \
                \readable copy is linked beside it." :: String)

    -- Jump links, with counts, so the shape of the list is visible at once.
    H.p H.! A.class_ "bib-jump" $
      mapM_ jumpLink (filter (not . null . thd) grouped)

    mapM_ renderSection grouped

    H.script H.! A.src "https://code.jquery.com/jquery-3.5.1.slim.min.js"
             H.! H.customAttribute "crossorigin" "anonymous" $ ""
    H.script H.! A.src "https://cdn.jsdelivr.net/npm/@popperjs/core@2.9.2/dist/umd/popper.min.js"
             H.! H.customAttribute "crossorigin" "anonymous" $ ""
    H.script H.! A.src "https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/js/bootstrap.min.js"
             H.! H.customAttribute "crossorigin" "anonymous" $ ""
  where
    grouped =
      [ (heading, anchor, sortOn (Down . entryYear)
                            (filter ((== kind) . kindOf) entries))
      | (heading, anchor, kind) <- sections ]

    thd (_, _, xs) = xs

    jumpLink (heading, anchor, es) = do
      H.a H.! A.href (H.toValue ('#' : anchor))
          H.! A.class_ "bib-jump-link" $
        H.toHtml (heading ++ " (" ++ show (length es) ++ ")")
      H.toHtml (" " :: String)

    renderSection (heading, anchor, es)
      | null es   = return ()
      | otherwise = H.div H.! A.class_ "bib-section" $ do
          H.h2 H.! A.class_ "bib-section-heading"
               H.! A.id (H.toValue anchor) $ H.toHtml heading
          H.ul H.! A.class_ "bib-list" $ mapM_ (renderEntry showBib) es
