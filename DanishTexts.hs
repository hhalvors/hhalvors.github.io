{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

-- | DanishTexts.hs
-- Generates the HTML catalog page for the Danish Philosophical Texts collection.
-- Data is read from data/danish-texts.yaml (symlinked from ~/danish-texts/catalog.yaml).

module DanishTexts (generateDanishTextsHTML, catalogFacts) where

import GHC.Generics               (Generic)
import Data.Aeson                  (FromJSON(..), Options, genericParseJSON,
                                    defaultOptions, fieldLabelModifier)
import Data.Char                   (toLower, isUpper)
import Data.List                   (intercalate, isPrefixOf)
import Data.Maybe                  (fromMaybe)
import qualified Data.Map.Strict             as M
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.String as R

import DanishNotes (NoteIndex, CatalogFacts(..), Target(..),
                    notesForWork, renderNoteLinks)

------------------------------------------------------------------------
-- Data types
------------------------------------------------------------------------

data Catalog = Catalog
  { authors    :: [Author]
  , references :: Maybe [Reference]   -- optional: catalog-wide reference works
  } deriving (Generic, Show)

data Author = Author
  { authorId   :: String
  , authorName :: String
  , authorDates :: String
  , authorBio  :: String
  , works      :: [Work]
  , authorModernEditions :: Maybe [Edition]         -- optional modern editions/reprints
  , authorSecondaryLiterature :: Maybe [SecondaryLit]  -- optional scholarship on the author
  , authorBibliography :: Maybe Bibliography   -- optional full publication list
  } deriving (Generic, Show)

-- A complete author bibliography (distinct from the curated `works` above):
-- published works + unpublished manuscripts.
data Bibliography = Bibliography
  { bibPublished   :: Maybe [BibEntry]
  , bibManuscripts :: Maybe [BibEntry]
  } deriving (Generic, Show)

data BibEntry = BibEntry
  { entryYear         :: String
  , entryTitle        :: String
  , entryVenue        :: Maybe String
  , entryNote         :: Maybe String
  , entryIncollection :: Maybe String   -- workId of a matching curated work, if any
  } deriving (Generic, Show)

data Work = Work
  { workId       :: String
  , workTitle    :: String
  , workYear     :: String
  , workVenue    :: Maybe String
  , workNote     :: Maybe String
  , workSections :: [Section]
  } deriving (Generic, Show)

data Section = Section
  { sectionTitle  :: String
  , sectionStatus :: String   -- raw string; rendered via statusBadge
  , sectionLinks  :: [Link]
  } deriving (Generic, Show)

-- A modern (typically twentieth-century) edition or reprint of one or more of
-- the author's `works`. Not a source of new text; listed for citation and for
-- whatever editorial apparatus it carries. `edEditor` is the modern editor.
data Edition = Edition
  { edId       :: String
  , edEditor   :: Maybe String
  , edTitle    :: String
  , edYear     :: String
  , edVenue    :: Maybe String
  , edNote     :: Maybe String
  , edSections :: [Section]
  } deriving (Generic, Show)

-- Secondary literature (scholarship) on an author. Distinct from `works`,
-- which are primary texts by the author. `secAuthor` is the scholar.
data SecondaryLit = SecondaryLit
  { secId       :: String
  , secAuthor   :: String
  , secTitle    :: String
  , secYear     :: String
  , secVenue    :: Maybe String
  , secDoi      :: Maybe String
  , secNote     :: Maybe String
  , secSections :: [Section]
  } deriving (Generic, Show)

data Link = Link
  { linkLabel :: String
  , linkUrl   :: String
  } deriving (Generic, Show)

-- A catalog-wide reference work (history/survey of Danish philosophy as a
-- whole), not tied to a single author. Rendered in its own section after the
-- authors. `refVolumes` optionally lists the individual volumes of a series.
data Reference = Reference
  { refId      :: String
  , refTitle   :: String
  , refAuthors :: String
  , refYear    :: String
  , refVenue   :: Maybe String
  , refNote    :: Maybe String
  , refVolumes :: Maybe [String]
  , refLinks   :: Maybe [Link]
  } deriving (Generic, Show)

------------------------------------------------------------------------
-- JSON/YAML field name mapping
-- YAML uses snake_case (author-id, work-title…); we strip the prefix.
------------------------------------------------------------------------

-- Convert camelCase field names to kebab-case for YAML keys.
camelToKebab :: String -> String
camelToKebab []     = []
camelToKebab (c:cs)
  | isUpper c = '-' : toLower c : camelToKebab cs
  | otherwise = c : camelToKebab cs

-- Strip a prefix from a field name (used to remove the type prefix).
stripPrefix :: String -> String -> String
stripPrefix p s
  | p `isPrefixOf` s = drop (length p) s
  | otherwise        = s

-- Lowercase the first character (needed after prefix-stripping leaves e.g. "Id").
lcFirst :: String -> String
lcFirst []     = []
lcFirst (c:cs) = toLower c : cs

opts :: String -> Options
opts pfx = defaultOptions
  { fieldLabelModifier = camelToKebab . lcFirst . stripPrefix pfx }

instance FromJSON Catalog where
  parseJSON = genericParseJSON (defaultOptions)

instance FromJSON Author where
  parseJSON = genericParseJSON (opts "author")

instance FromJSON Work where
  parseJSON = genericParseJSON (opts "work")

instance FromJSON Section where
  parseJSON = genericParseJSON (opts "section")

instance FromJSON Edition where
  parseJSON = genericParseJSON (opts "ed")

instance FromJSON SecondaryLit where
  parseJSON = genericParseJSON (opts "sec")

instance FromJSON Link where
  parseJSON = genericParseJSON (opts "link")

instance FromJSON Reference where
  parseJSON = genericParseJSON (opts "ref")

instance FromJSON Bibliography where
  parseJSON = genericParseJSON (opts "bib")

instance FromJSON BibEntry where
  parseJSON = genericParseJSON (opts "entry")

------------------------------------------------------------------------
-- Status badge rendering
------------------------------------------------------------------------

statusClass :: String -> String
statusClass "complete"     = "dt-status dt-status-complete"
statusClass "in-progress"  = "dt-status dt-status-inprogress"
statusClass "skeleton"     = "dt-status dt-status-skeleton"
statusClass "to-do"        = "dt-status dt-status-todo"
statusClass "coming-soon"  = "dt-status dt-status-soon"
statusClass "placeholder"  = "dt-status dt-status-placeholder"
statusClass "reference"    = "dt-status dt-status-reference"
statusClass _              = "dt-status dt-status-todo"

statusLabel :: String -> String
statusLabel "complete"     = "complete"
statusLabel "in-progress"  = "in progress"
statusLabel "skeleton"     = "skeleton"
statusLabel "to-do"        = "to do"
statusLabel "coming-soon"  = "coming soon"
statusLabel "placeholder"  = "placeholder"
statusLabel "reference"    = "reference"
statusLabel s              = s

-- "none" suppresses the badge entirely. The YAML parser requires every section
-- to carry a `status` key, so a section that should show no badge says
-- `status: none` rather than omitting it. Used by the Bohr edition, where all
-- 89 sections are in the same state and 89 identical badges are only clutter.
statusBadge :: String -> H.Html
statusBadge "none" = return ()
statusBadge s =
  H.span H.! A.class_ (H.toValue $ statusClass s)
         $ H.toHtml (statusLabel s)

------------------------------------------------------------------------
-- Link badge rendering
------------------------------------------------------------------------

linkBadge :: Link -> H.Html
linkBadge lnk =
  H.a H.! A.href   (H.toValue $ linkUrl lnk)
      H.! A.class_ "dt-link"
      H.! A.target "_blank"
      H.! A.rel    "noopener noreferrer"
      $ H.toHtml (linkLabel lnk)

------------------------------------------------------------------------
-- Section row
------------------------------------------------------------------------

renderSection :: Section -> H.Html
renderSection sec =
  H.div H.! A.class_ "dt-section" $ do
    H.span H.! A.class_ "dt-section-title"
           $ H.toHtml (sectionTitle sec)
    H.span H.! A.class_ "dt-section-links" $ do
      mapM_ linkBadge (sectionLinks sec)
      statusBadge (sectionStatus sec)

------------------------------------------------------------------------
-- Work entry
------------------------------------------------------------------------

renderWork :: NoteIndex -> String -> Work -> H.Html
renderWork idx aid w =
  H.div H.! A.class_ "dt-work"
        H.! A.id (H.toValue $ aid ++ "-" ++ workId w) $ do
    H.div H.! A.class_ "dt-work-title" $ H.toHtml (workTitle w)
    let venue = fromMaybe "" (workVenue w)
        year  = workYear w
        meta  = case (year, venue) of
                  ("", "")  -> ""
                  ("", v)   -> v
                  (y,  "")  -> y
                  (y,   v)  -> y ++ ". " ++ v
    if null meta
      then return ()
      else H.div H.! A.class_ "dt-work-meta" $ H.toHtml meta
    case workNote w of
      Just n  -> H.div H.! A.class_ "dt-work-note" $ H.toHtml n
      Nothing -> return ()
    H.div H.! A.class_ "dt-sections" $
      mapM_ renderSection (workSections w)
    -- Notes and essays are discovered from the danish-texts tree, not
    -- declared here: catalog.yaml has no field for them. See DanishNotes.hs.
    renderNoteLinks (notesForWork aid (workId w) idx)

------------------------------------------------------------------------
-- Modern edition entry
------------------------------------------------------------------------

-- Meta line reads "Ed. A. H. Winsnes. 1966. Oslo: Tanum…", with the editor
-- and venue both optional.
renderEdition :: String -> Edition -> H.Html
renderEdition aid e =
  H.div H.! A.class_ "dt-work dt-edition"
        H.! A.id (H.toValue $ aid ++ "-" ++ edId e) $ do
    H.div H.! A.class_ "dt-work-title" $ H.toHtml (edTitle e)
    let venue = fromMaybe "" (edVenue e)
        pub   = case (edYear e, venue) of
                  ("", "")  -> ""
                  ("", v)   -> v
                  (y,  "")  -> y
                  (y,   v)  -> y ++ ". " ++ v
        meta  = case edEditor e of
                  Just ed | not (null ed) ->
                    "Ed. " ++ ed ++ if null pub then "" else ". " ++ pub
                  _ -> pub
    if null meta
      then return ()
      else H.div H.! A.class_ "dt-work-meta" $ H.toHtml meta
    case edNote e of
      Just n  -> H.div H.! A.class_ "dt-work-note" $ H.toHtml n
      Nothing -> return ()
    H.div H.! A.class_ "dt-sections" $
      mapM_ renderSection (edSections e)

------------------------------------------------------------------------
-- Secondary literature entry
------------------------------------------------------------------------

renderSecondaryLit :: String -> SecondaryLit -> H.Html
renderSecondaryLit aid s =
  H.div H.! A.class_ "dt-work dt-seclit"
        H.! A.id (H.toValue $ aid ++ "-" ++ secId s) $ do
    H.div H.! A.class_ "dt-work-title" $ H.toHtml (secTitle s)
    let venue = fromMaybe "" (secVenue s)
        year  = secYear s
        pub   = case (year, venue) of
                  ("", "")  -> ""
                  ("", v)   -> v
                  (y,  "")  -> y
                  (y,   v)  -> y ++ ". " ++ v
        meta  = secAuthor s ++ if null pub then "" else ". " ++ pub
    H.div H.! A.class_ "dt-work-meta" $ H.toHtml meta
    case secDoi s of
      Just d  -> H.div H.! A.class_ "dt-work-meta" $ do
                   "DOI: "
                   H.a H.! A.href (H.toValue $ "https://doi.org/" ++ d)
                       H.! A.target "_blank"
                       H.! A.rel "noopener noreferrer"
                       $ H.toHtml d
      Nothing -> return ()
    case secNote s of
      Just n  -> H.div H.! A.class_ "dt-work-note" $ H.toHtml n
      Nothing -> return ()
    H.div H.! A.class_ "dt-sections" $
      mapM_ renderSection (secSections s)

------------------------------------------------------------------------
-- Reference work entry (catalog-wide surveys)
------------------------------------------------------------------------

renderReference :: Reference -> H.Html
renderReference r =
  H.div H.! A.class_ "dt-work dt-seclit dt-reference"
        H.! A.id (H.toValue $ "ref-" ++ refId r) $ do
    H.div H.! A.class_ "dt-work-title" $ H.toHtml (refTitle r)
    let venue = fromMaybe "" (refVenue r)
        pub   = case (refYear r, venue) of
                  ("", "")  -> ""
                  ("", v)   -> v
                  (y,  "")  -> y
                  (y,   v)  -> y ++ ". " ++ v
        meta  = refAuthors r ++ if null pub then "" else ". " ++ pub
    H.div H.! A.class_ "dt-work-meta" $ H.toHtml meta
    case refNote r of
      Just n  -> H.div H.! A.class_ "dt-work-note" $ H.toHtml n
      Nothing -> return ()
    case refVolumes r of
      Just vs | not (null vs) ->
        H.ul H.! A.class_ "dt-ref-volumes" $ mapM_ (H.li . H.toHtml) vs
      _ -> return ()
    H.div H.! A.class_ "dt-section" $
      H.span H.! A.class_ "dt-section-links" $ do
        mapM_ linkBadge (fromMaybe [] (refLinks r))
        statusBadge "reference"

------------------------------------------------------------------------
-- Full bibliography (collapsible)
------------------------------------------------------------------------

renderBibEntry :: String -> BibEntry -> H.Html
renderBibEntry aid e =
  H.div H.! A.class_ (H.toValue rowClass) $ do
    H.span H.! A.class_ "dt-bib-year" $ H.toHtml (entryYear e)
    H.span H.! A.class_ "dt-bib-cite" $ do
      H.span H.! A.class_ "dt-bib-title" $ H.toHtml (entryTitle e)
      case entryVenue e of
        Just v  -> H.span H.! A.class_ "dt-bib-venue" $ H.toHtml (" · " ++ v)
        Nothing -> return ()
      case entryNote e of
        Just n  -> H.span H.! A.class_ "dt-bib-note" $ H.toHtml (" — " ++ n)
        Nothing -> return ()
      case entryIncollection e of
        Just wid -> H.a H.! A.class_ "dt-bib-incoll"
                        H.! A.href (H.toValue $ "#" ++ aid ++ "-" ++ wid)
                        $ "✦ in the collection"
        Nothing  -> return ()
  where
    rowClass = "dt-bib-entry"
             ++ maybe "" (const " dt-bib-entry-incoll") (entryIncollection e)

bibCount :: Bibliography -> Int
bibCount b = length (fromMaybe [] (bibPublished b))
           + length (fromMaybe [] (bibManuscripts b))

renderBibliography :: String -> String -> Bibliography -> H.Html
renderBibliography aid name b =
  H.details H.! A.class_ "dt-bib"
            H.! A.id (H.toValue $ aid ++ "-bibliography") $ do
    H.summary H.! A.class_ "dt-bib-summary"
              $ H.toHtml ("Complete bibliography (" ++ show total ++ " items)")
    H.p H.! A.class_ "dt-bib-intro" $ do
      H.toHtml ("A near-complete list of " ++ name ++ "'s publications and manuscripts. ")
      H.span H.! A.class_ "dt-bib-key" $ "✦ marks works available in this collection."
    renderGroup "Published works" pub
    renderGroup "Unpublished manuscripts" mss
  where
    pub   = fromMaybe [] (bibPublished b)
    mss   = fromMaybe [] (bibManuscripts b)
    total = length pub + length mss
    renderGroup _     [] = return ()
    renderGroup label xs = do
      H.h3 H.! A.class_ "dt-bib-head" $ H.toHtml (label :: String)
      H.div H.! A.class_ "dt-bib-list" $ mapM_ (renderBibEntry aid) xs

------------------------------------------------------------------------
-- Author section
------------------------------------------------------------------------

renderAuthor :: NoteIndex -> Author -> H.Html
renderAuthor idx a =
  H.section H.! A.class_ "dt-author" H.! A.id (H.toValue $ authorId a) $ do
    H.h2 H.! A.class_ "dt-author-heading" $ do
      H.toHtml (authorName a)
      H.span H.! A.class_ "dt-author-dates"
             $ H.toHtml (" (" ++ authorDates a ++ ")")
    H.p H.! A.class_ "dt-author-bio" $ H.toHtml (authorBio a)
    mapM_ (renderWork idx (authorId a)) (works a)
    case authorModernEditions a of
      Just es | not (null es) -> do
        H.h3 H.! A.class_ "dt-seclit-head"
               H.! A.id (H.toValue $ authorId a ++ "-editions")
               $ "Modern editions"
        mapM_ (renderEdition (authorId a)) es
      _ -> return ()
    case authorSecondaryLiterature a of
      Just ss | not (null ss) -> do
        H.h3 H.! A.class_ "dt-seclit-head"
               H.! A.id (H.toValue $ authorId a ++ "-secondary")
               $ "Secondary literature"
        mapM_ (renderSecondaryLit (authorId a)) ss
      _ -> return ()
    case authorBibliography a of
      Just b  -> renderBibliography (authorId a) (authorName a) b
      Nothing -> return ()
    H.a H.! A.class_ "dt-backtomenu" H.! A.href "#browse" $ "↑ Browse"

------------------------------------------------------------------------
-- Browse menu
--
-- A collapsible index at the head of the page: one <details> per
-- philosopher, expanding to the list of that philosopher's works, each
-- title linking to the corresponding entry further down. Pure HTML/CSS —
-- no JavaScript.
------------------------------------------------------------------------

-- Menu titles are shortened to keep the two-column grid tidy; the full
-- title is kept in the link's title attribute.
shorten :: Int -> String -> String
shorten n s
  | length s <= n = s
  | otherwise     = trimRight (cutWord (take n s)) ++ "…"
  where
    cutWord t = case break (== ' ') (reverse t) of
                  (_, ' ' : rest) -> reverse rest
                  _               -> t
    trimRight = reverse . dropWhile (`elem` (" ,.;:-–—" :: String)) . reverse

-- A single menu row pointing at an anchor, with an optional leading year.
menuRow :: String -> String -> String -> String -> H.Html
menuRow anchor cls year title =
  H.li $ H.a H.! A.class_ (H.toValue cls)
             H.! A.href  (H.toValue $ "#" ++ anchor)
             H.! A.title (H.toValue title) $ do
    H.span H.! A.class_ "dt-menu-year"  $ H.toHtml year
    H.span H.! A.class_ "dt-menu-title" $ H.toHtml (shorten 52 title)

renderMenuAuthor :: Author -> H.Html
renderMenuAuthor a =
  H.details H.! A.class_ "dt-menu-author" $ do
    H.summary H.! A.class_ "dt-menu-summary" $ do
      H.span H.! A.class_ "dt-menu-name"  $ H.toHtml (authorName a)
      H.span H.! A.class_ "dt-menu-dates" $ H.toHtml (authorDates a)
      H.span H.! A.class_ "dt-menu-count" $ H.toHtml countLabel
    H.ul H.! A.class_ "dt-menu-works" $ do
      mapM_ workRow (works a)
      extraRows
  where
    aid = authorId a
    n   = length (works a)
    countLabel :: String
    countLabel
      | n == 0    = "—"
      | n == 1    = "1 work"
      | otherwise = show n ++ " works"

    workRow w = menuRow (aid ++ "-" ++ workId w) "dt-menu-link"
                        (workYear w) (workTitle w)

    extraRow anchor label = menuRow anchor "dt-menu-link dt-menu-extra" "" label

    extraRows = do
      case authorModernEditions a of
        Just es | not (null es) ->
          extraRow (aid ++ "-editions")
                   ("Modern editions (" ++ show (length es) ++ ")")
        _ -> return ()
      case authorSecondaryLiterature a of
        Just ss | not (null ss) ->
          extraRow (aid ++ "-secondary")
                   ("Secondary literature (" ++ show (length ss) ++ ")")
        _ -> return ()
      case authorBibliography a of
        Just b ->
          extraRow (aid ++ "-bibliography")
                   ("Complete bibliography (" ++ show (bibCount b) ++ " items)")
        Nothing -> return ()
      extraRow aid ("All " ++ authorName a ++ " entries ↓")

renderMenuReferences :: [Reference] -> H.Html
renderMenuReferences rs =
  H.details H.! A.class_ "dt-menu-author" $ do
    H.summary H.! A.class_ "dt-menu-summary" $ do
      H.span H.! A.class_ "dt-menu-name"  $ "General reference works"
      H.span H.! A.class_ "dt-menu-dates" $ "surveys"
      H.span H.! A.class_ "dt-menu-count" $ H.toHtml refCount
    H.ul H.! A.class_ "dt-menu-works" $
      mapM_ (\r -> menuRow ("ref-" ++ refId r) "dt-menu-link"
                           (refYear r) (refTitle r)) rs
  where
    refCount :: String
    refCount | length rs == 1 = "1 item"
             | otherwise      = show (length rs) ++ " items"

renderMenu :: Catalog -> H.Html
renderMenu cat =
  H.nav H.! A.class_ "dt-menu" H.! A.id "browse" $ do
    H.h2 H.! A.class_ "dt-menu-head" $ "Browse the collection"
    H.p H.! A.class_ "dt-menu-hint" $ H.toHtml hint
    H.div H.! A.class_ "dt-menu-grid" $ do
      mapM_ renderMenuAuthor (authors cat)
      case references cat of
        Just rs | not (null rs) -> renderMenuReferences rs
        _ -> return ()
  where
    nAuthors = length (authors cat)
    nWorks   = sum (map (length . works) (authors cat))
    hint :: String
    hint = show nAuthors ++ " philosophers · " ++ show nWorks
         ++ " works. Click a name to see the works, then a title to jump to it."

------------------------------------------------------------------------
-- Top-level generator
------------------------------------------------------------------------

-- | The author names and work titles the notes machinery needs, so that
-- DanishNotes can render its index without knowing the catalog's types.
catalogFacts :: Catalog -> CatalogFacts
catalogFacts cat = CatalogFacts
  { cfAuthorNames = [ (authorId a, authorName a) | a <- authors cat ]
  , cfWorkTitles  = M.fromList
      [ (Target (authorId a) (workId w), workTitle w)
      | a <- authors cat, w <- works a ]
  }

generateDanishTextsHTML :: NoteIndex -> Catalog -> String
generateDanishTextsHTML idx catalog = R.renderHtml $
  H.div H.! A.class_ "dt-catalog" $ do
    H.p H.! A.class_ "dt-intro" $ do
      "Transcriptions and English translations of Danish philosophical works, "
      "primarily from the nineteenth century. All source texts are in the public domain; "
      "scans are drawn from the "
      H.a H.! A.href "https://www.kb.dk/en"
          H.! A.target "_blank" $ "Royal Danish Library"
      ", the "
      H.a H.! A.href "https://www.nb.no/"
          H.! A.target "_blank" $ "National Library of Norway"
      ", and the "
      H.a H.! A.href "https://archive.org/"
          H.! A.target "_blank" $ "Internet Archive"
      ". LaTeX sources and PDF files are on "
      H.a H.! A.href "https://github.com/hhalvors/danish-texts"
          H.! A.target "_blank" $ "GitHub"
      ". For scholarly commentary on each text, see the "
      H.a H.! A.href "/dansk/notes.html" $ "notes page"
      "."
    renderMenu catalog
    H.p H.! A.class_ "dt-intro dt-note" $ do
      "A note on the word “Danish”. Denmark and Norway were joined under a single "
      "crown from 1380 and administered as one state from the sixteenth century "
      "until 1814, governed from Copenhagen — which held the only university of the "
      "joint realms until the Royal Frederick University was founded in Christiania "
      "in 1811. Danish was the written language of educated Norwegians no less than "
      "of Danes. Ludvig Holberg, born in Bergen, and Niels Treschow, born in "
      "Drammen, are accordingly claimed today by Norway as well as by Denmark; but "
      "neither faced a choice between two national literatures, and for their own "
      "lifetimes the question does not arise. Treschow is the exact hinge: he held "
      "the chair of philosophy at Copenhagen until 1813, moved to the new university "
      "in Christiania, and saw the union dissolved by the Treaty of Kiel in January "
      "1814. “Danish” here therefore means the philosophy of that shared realm and "
      "its shared written language, rather than of the post-1814 nation-state."
    H.div H.! A.class_ "dt-legend" $ do
      H.span H.! A.class_ "dt-legend-label" $ "Status: "
      statusBadge "complete"
      statusBadge "in-progress"
      statusBadge "skeleton"
      statusBadge "to-do"
      statusBadge "coming-soon"
      statusBadge "reference"
    mapM_ (renderAuthor idx) (authors catalog)
    case references catalog of
      Just rs | not (null rs) ->
        H.section H.! A.class_ "dt-author dt-references" H.! A.id "references" $ do
          H.h2 H.! A.class_ "dt-author-heading" $ "General reference works"
          H.p H.! A.class_ "dt-author-bio" $
            H.toHtml ("Histories and surveys of Danish philosophy as a whole. \
                      \These are secondary literature, listed for reference; they \
                      \are not part of the transcription and translation program." :: String)
          mapM_ renderReference rs
      _ -> return ()
