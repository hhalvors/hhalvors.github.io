{-# LANGUAGE OverloadedStrings #-}

-- | EquivBiblio.hs
-- Generates the HTML bibliography page for "Equivalent Theories".

module EquivBiblio (generateEquivHTML) where

import Text.BibTeX.Entry    (T(..))
import Data.List            (intercalate, isPrefixOf)
import Data.Maybe           (fromMaybe)
import Data.Char            (isAlpha, isSpace)
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.String as R

------------------------------------------------------------------------
-- Sections: (display heading, value of the `keywords` field)
------------------------------------------------------------------------

sections :: [(String, String)]
sections =
  [ ("Surveys and Books",               "surveys")
  , ("Foundational Logic",              "foundational")
  , ("Formal Criteria for Equivalence", "criteria")
  , ("Philosophy of Physics",           "physics")
  ]

------------------------------------------------------------------------
-- Field accessors
------------------------------------------------------------------------

getField :: String -> T -> Maybe String
getField k (Cons _ _ fs) = lookup k fs

getField' :: String -> T -> String
getField' k e = fromMaybe "" (getField k e)

bibKey :: T -> String
bibKey (Cons _ k _) = k

bibType :: T -> String
bibType (Cons t _ _) = t

------------------------------------------------------------------------
-- LaTeX → plain-text sanitizer
-- Handles: brace stripping, accent commands, -- and --- dashes,
--          and silently drops unknown \commands.
------------------------------------------------------------------------

sanitize :: String -> String
sanitize []                              = []
sanitize ('{' : xs)                      = sanitize xs
sanitize ('}' : xs)                      = sanitize xs
sanitize ('-':'-':'-': xs)               = '\8212' : sanitize xs  -- em-dash
sanitize ('-':'-'    : xs)               = '\8211' : sanitize xs  -- en-dash
-- Acute accent: \'x
sanitize ('\\':'\'' : c : xs)            = withAccent acuteMap  c xs
-- Grave accent: \`x
sanitize ('\\':'`'  : c : xs)            = withAccent graveMap  c xs
-- Umlaut/diaeresis: \"x
sanitize ('\\':'"'  : c : xs)            = withAccent umlautMap c xs
-- Circumflex: \^x
sanitize ('\\':'^'  : c : xs)            = withAccent circumMap c xs
-- Tilde: \~x
sanitize ('\\':'~'  : c : xs)            = withAccent tildeMap  c xs
-- Cedilla: \c{x}  (must come before the general \alpha fallback)
sanitize ('\\':'c':'{': c :'}': xs)      =
  fromMaybe c (lookup c cedillaMap) : sanitize xs
-- Skip any other \command (e.g. \emph, \textit)
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
-- Author formatting
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

formatAuthors :: String -> String
formatAuthors raw =
  let names = map (invertName . trim) (splitByAnd (sanitize raw))
  in case names of
    []     -> ""
    [x]    -> x
    [x, y] -> x ++ " and " ++ y
    xs     -> intercalate ", " (init xs) ++ ", and " ++ last xs

------------------------------------------------------------------------
-- Venue formatting
------------------------------------------------------------------------

formatVenue :: T -> String
formatVenue e =
  let j   = sanitize <$> getField "journal"   e
      bt  = sanitize <$> getField "booktitle"  e
      pb  = sanitize <$> getField "publisher"  e
      vol = getField "volume"  e
      num = getField "number"  e
      pgs = sanitize <$> getField "pages"      e
  in case bibType e of
    "book"          -> fromMaybe "" pb
    "incollection"  ->
      "In " ++ fromMaybe "?" bt ++
      (case getField "editor" e of
         Just ed -> ", ed. " ++ sanitize ed
         Nothing -> "") ++
      ". " ++ fromMaybe "" pb
    "inproceedings" -> "In " ++ fromMaybe "?" bt
    _               ->
      case j of
        Just jname -> jname ++ volNumPages vol num pgs
        Nothing    -> fromMaybe (fromMaybe "" pb) bt
  where
    volNumPages v n p =
      maybe "" (\x -> " " ++ x) v
      ++ maybe "" (\x -> "(" ++ x ++ ")") n
      ++ maybe "" (\x -> ": " ++ x) p

------------------------------------------------------------------------
-- BibTeX string for the collapsible snippet
-- Omits site-internal fields.
------------------------------------------------------------------------

bibtexString :: T -> String
bibtexString (Cons et key fs) =
  "@" ++ et ++ "{" ++ key ++ ",\n"
  ++ concatMap showField (filter ((`notElem` internal) . fst) fs)
  ++ "}\n"
  where
    internal     = ["philsci", "philpapers", "keywords", "isbn"]
    showField (k, v) = "  " ++ k ++ " = {" ++ v ++ "},\n"

------------------------------------------------------------------------
-- Link badges
------------------------------------------------------------------------

badge :: String -> String -> String -> H.Html
badge cls label url =
  H.a H.! A.href   (H.toValue url)
      H.! A.class_ (H.toValue cls)
      H.! A.target "_blank"
      H.! A.rel    "noopener noreferrer"
      $ H.toHtml label

doiBadge :: String -> H.Html
doiBadge doi = badge "bib-badge bib-badge-doi" "DOI"
               ("https://doi.org/" ++ doi)

preprintBadge :: String -> H.Html
preprintBadge url = badge "bib-badge bib-badge-pre" "Preprint" url

philSciBadge :: String -> H.Html
philSciBadge url = badge "bib-badge bib-badge-pre" "PhilSci" url

philPapersBadge :: String -> H.Html
philPapersBadge url = badge "bib-badge bib-badge-pre" "PhilPapers" url

bibtexToggle :: String -> H.Html
bibtexToggle key =
  H.a H.! A.class_ "bib-badge bib-badge-bib"
      H.! A.role   "button"
      H.! H.customAttribute "data-toggle"   "collapse"
      H.! H.customAttribute "aria-expanded" "false"
      H.! A.href   (H.toValue $ "#bib-" ++ key)
      $ "BibTeX"

------------------------------------------------------------------------
-- Single entry renderer
------------------------------------------------------------------------

renderEntry :: T -> H.Html
renderEntry e = H.li H.! A.class_ "bib-entry" $ do

  H.span H.! A.class_ "bib-authors"
    $ H.toHtml (formatAuthors (getField' "author" e))
  H.toHtml (" (" :: String)
  H.span H.! A.class_ "bib-year"
    $ H.toHtml (getField' "year" e)
  H.toHtml ("). " :: String)

  -- Title: linked to DOI or preprint URL if available
  let titleStr = sanitize (getField' "title" e)
  H.span H.! A.class_ "bib-title" $
    case (getField "doi" e, getField "url" e) of
      (Just doi, _) ->
        H.a H.! A.href (H.toValue $ "https://doi.org/" ++ doi)
            H.! A.target "_blank" $ H.toHtml titleStr
      (Nothing, Just url) ->
        H.a H.! A.href (H.toValue url)
            H.! A.target "_blank" $ H.toHtml titleStr
      _ -> H.toHtml titleStr

  -- Venue
  let venue = formatVenue e
  if null venue
    then H.toHtml ("." :: String)
    else do H.toHtml (". " :: String)
            H.em $ H.toHtml venue
            H.toHtml ("." :: String)

  -- Link badges
  H.span H.! A.class_ "bib-links" $ do
    maybe (return ()) doiBadge        (getField "doi"        e)
    maybe (return ()) preprintBadge   (getField "url"        e)
    maybe (return ()) philSciBadge    (getField "philsci"    e)
    maybe (return ()) philPapersBadge (getField "philpapers" e)
    bibtexToggle (bibKey e)

  -- Collapsible BibTeX
  H.div H.! A.class_ "collapse"
        H.! A.id (H.toValue $ "bib-" ++ bibKey e) $
    H.div H.! A.class_ "bib-bibtex-block" $
      H.pre $ H.code $ H.toHtml (bibtexString e)

  -- Optional annotation
  case getField "annote" e of
    Just note -> H.p H.! A.class_ "bib-annote" $ H.toHtml (sanitize note)
    Nothing   -> return ()

------------------------------------------------------------------------
-- Section renderer
------------------------------------------------------------------------

renderSection :: String -> [T] -> H.Html
renderSection heading entries = do
  H.h2 H.! A.class_ "bib-section-heading" $ H.toHtml heading
  H.ul H.! A.class_ "bib-list" $ mapM_ renderEntry entries

------------------------------------------------------------------------
-- Top-level generator
------------------------------------------------------------------------

generateEquivHTML :: [T] -> String
generateEquivHTML entries = R.renderHtml $
  H.div H.! A.class_ "equiv-biblio" $ do
    H.p H.! A.class_ "bib-intro" $
      "A working bibliography on theoretical equivalence in logic and \
      \philosophy of science, with links to published versions and \
      \freely accessible preprints. Entries are grouped thematically."
    mapM_ renderOneSection sections
    H.script H.! A.src "https://code.jquery.com/jquery-3.5.1.slim.min.js"
             H.! H.customAttribute "crossorigin" "anonymous" $ ""
    H.script H.! A.src "https://cdn.jsdelivr.net/npm/@popperjs/core@2.9.2/dist/umd/popper.min.js"
             H.! H.customAttribute "crossorigin" "anonymous" $ ""
    H.script H.! A.src "https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/js/bootstrap.min.js"
             H.! H.customAttribute "crossorigin" "anonymous" $ ""
  where
    renderOneSection (heading, kw) =
      let es = filter (\e -> getField "keywords" e == Just kw) entries
      in if null es then return () else renderSection heading es
