{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

-- | DanishTexts.hs
-- Generates the HTML catalog page for the Danish Philosophical Texts collection.
-- Data is read from data/danish-texts.yaml (symlinked from ~/danish-texts/catalog.yaml).

module DanishTexts (generateDanishTextsHTML) where

import GHC.Generics               (Generic)
import Data.Aeson                  (FromJSON(..), Options, genericParseJSON,
                                    defaultOptions, fieldLabelModifier)
import Data.Char                   (toLower, isUpper)
import Data.List                   (intercalate, isPrefixOf)
import Data.Maybe                  (fromMaybe)
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.String as R

------------------------------------------------------------------------
-- Data types
------------------------------------------------------------------------

data Catalog = Catalog
  { authors :: [Author]
  } deriving (Generic, Show)

data Author = Author
  { authorId   :: String
  , authorName :: String
  , authorDates :: String
  , authorBio  :: String
  , works      :: [Work]
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

instance FromJSON SecondaryLit where
  parseJSON = genericParseJSON (opts "sec")

instance FromJSON Link where
  parseJSON = genericParseJSON (opts "link")

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

statusBadge :: String -> H.Html
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

renderWork :: String -> Work -> H.Html
renderWork aid w =
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

renderBibliography :: String -> String -> Bibliography -> H.Html
renderBibliography aid name b =
  H.details H.! A.class_ "dt-bib" $ do
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

renderAuthor :: Author -> H.Html
renderAuthor a =
  H.section H.! A.class_ "dt-author" H.! A.id (H.toValue $ authorId a) $ do
    H.h2 H.! A.class_ "dt-author-heading" $ do
      H.toHtml (authorName a)
      H.span H.! A.class_ "dt-author-dates"
             $ H.toHtml (" (" ++ authorDates a ++ ")")
    H.p H.! A.class_ "dt-author-bio" $ H.toHtml (authorBio a)
    mapM_ (renderWork (authorId a)) (works a)
    case authorSecondaryLiterature a of
      Just ss | not (null ss) -> do
        H.h3 H.! A.class_ "dt-seclit-head" $ "Secondary literature"
        mapM_ (renderSecondaryLit (authorId a)) ss
      _ -> return ()
    case authorBibliography a of
      Just b  -> renderBibliography (authorId a) (authorName a) b
      Nothing -> return ()

------------------------------------------------------------------------
-- Top-level generator
------------------------------------------------------------------------

generateDanishTextsHTML :: Catalog -> String
generateDanishTextsHTML catalog = R.renderHtml $
  H.div H.! A.class_ "dt-catalog" $ do
    H.p H.! A.class_ "dt-intro" $ do
      "Transcriptions and English translations of Danish philosophical works, "
      "primarily from the nineteenth century. All source texts are in the public domain; "
      "scans are drawn from the "
      H.a H.! A.href "https://www.kb.dk/en"
          H.! A.target "_blank" $ "Royal Danish Library"
      ". LaTeX sources and PDF files are on "
      H.a H.! A.href "https://github.com/hhalvors/danish-texts"
          H.! A.target "_blank" $ "GitHub"
      ". For scholarly commentary on each text, see the "
      H.a H.! A.href "/danish-texts/notes.html" $ "notes page"
      "."
    H.div H.! A.class_ "dt-legend" $ do
      H.span H.! A.class_ "dt-legend-label" $ "Status: "
      statusBadge "complete"
      statusBadge "in-progress"
      statusBadge "skeleton"
      statusBadge "to-do"
      statusBadge "coming-soon"
      statusBadge "reference"
    mapM_ renderAuthor (authors catalog)
