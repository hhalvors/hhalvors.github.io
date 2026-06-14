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

instance FromJSON Link where
  parseJSON = genericParseJSON (opts "link")

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
statusClass _              = "dt-status dt-status-todo"

statusLabel :: String -> String
statusLabel "complete"     = "complete"
statusLabel "in-progress"  = "in progress"
statusLabel "skeleton"     = "skeleton"
statusLabel "to-do"        = "to do"
statusLabel "coming-soon"  = "coming soon"
statusLabel "placeholder"  = "placeholder"
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

renderWork :: Work -> H.Html
renderWork w =
  H.div H.! A.class_ "dt-work" $ do
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
    mapM_ renderWork (works a)

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
    mapM_ renderAuthor (authors catalog)
