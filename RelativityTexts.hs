{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

-- | RelativityTexts.hs
-- Generates the catalog page /relativity/ for the Relativity Texts collection
-- (transcriptions and translations of early papers on relativity theory).
-- Data is read from data/relativity-texts.yaml, a symlink to
-- ~/relativity-texts/catalog.yaml. Trimmed from DanishTexts.hs (2026-10-20):
-- same schema and the same dt-* CSS classes (css/style.css), without the notes
-- machinery, modern editions, secondary literature, bibliographies, references,
-- dossiers and browse menu, none of which this collection has.
--
-- Fields beyond the danish-texts schema:
--   title-en  English title, shown under the original title
--   source    full citation of the printing transcribed; shown instead of
--             "year. venue" when present
--   publish   false leaves the work off the page (its PDFs are kept out of the
--             public build by PRIVATE in ~/relativity-texts/Makefile)
--   phase     1 = the papers being edited now; 2 = listed under "Planned"
-- ~/relativity-texts/validate-catalog.py checks the file against these types.

module RelativityTexts (generateRelativityTextsHTML) where

import GHC.Generics               (Generic)
import Control.Monad               (forM_, unless)
import Data.Aeson                  (FromJSON(..), Options, genericParseJSON,
                                    defaultOptions, fieldLabelModifier)
import Data.Char                   (toLower, isUpper)
import Data.List                   (isPrefixOf)
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
  { authorId    :: String
  , authorName  :: String
  , authorDates :: String
  , authorBio   :: String
  , works       :: [Work]
  } deriving (Generic, Show)

data Work = Work
  { workId       :: String
  , workTitle    :: String
  , workTitleEn  :: Maybe String
  , workYear     :: String
  , workVenue    :: Maybe String
  , workSource   :: Maybe String
  , workNote     :: Maybe String
  , workPublish  :: Maybe Bool
  , workPhase    :: Maybe Int
  , workSections :: [Section]
  } deriving (Generic, Show)

data Section = Section
  { sectionTitle  :: String
  , sectionStatus :: String
  , sectionLinks  :: [Link]
  } deriving (Generic, Show)

data Link = Link
  { linkLabel :: String
  , linkUrl   :: String
  } deriving (Generic, Show)

------------------------------------------------------------------------
-- YAML field names: strip the type prefix, then camelCase -> kebab-case
-- (workTitleEn -> title-en), as in DanishTexts.hs.
------------------------------------------------------------------------

camelToKebab :: String -> String
camelToKebab []     = []
camelToKebab (c:cs)
  | isUpper c = '-' : toLower c : camelToKebab cs
  | otherwise = c : camelToKebab cs

stripPrefix :: String -> String -> String
stripPrefix p s
  | p `isPrefixOf` s = drop (length p) s
  | otherwise        = s

lcFirst :: String -> String
lcFirst []     = []
lcFirst (c:cs) = toLower c : cs

opts :: String -> Options
opts pfx = defaultOptions
  { fieldLabelModifier = camelToKebab . lcFirst . stripPrefix pfx }

instance FromJSON Catalog where
  parseJSON = genericParseJSON defaultOptions

instance FromJSON Author where
  parseJSON = genericParseJSON (opts "author")

instance FromJSON Work where
  parseJSON = genericParseJSON (opts "work")

instance FromJSON Section where
  parseJSON = genericParseJSON (opts "section")

instance FromJSON Link where
  parseJSON = genericParseJSON (opts "link")

------------------------------------------------------------------------
-- Which works are shown, and where
------------------------------------------------------------------------

published :: Work -> Bool
published w = workPublish w /= Just False

planned :: Work -> Bool
planned w = workPhase w == Just 2

-- Works of the current phase, per author; authors with none are not shown
-- in the main list (their planned works appear under "Planned").
current :: Author -> [Work]
current a = [ w | w <- works a, published w, not (planned w) ]

------------------------------------------------------------------------
-- Badges and links (same classes as the Danish Texts page)
------------------------------------------------------------------------

statusClass :: String -> String
statusClass "complete"    = "dt-status dt-status-complete"
statusClass "in-progress" = "dt-status dt-status-inprogress"
statusClass _             = "dt-status dt-status-todo"

statusLabel :: String -> String
statusLabel "complete"    = "complete"
statusLabel "in-progress" = "in progress"
statusLabel "to-do"       = "to do"
statusLabel s             = s

statusBadge :: String -> H.Html
statusBadge "none" = return ()
statusBadge s =
  H.span H.! A.class_ (H.toValue $ statusClass s) $ H.toHtml (statusLabel s)

linkBadge :: Link -> H.Html
linkBadge lnk =
  H.a H.! A.href   (H.toValue $ linkUrl lnk)
      H.! A.class_ "dt-link"
      H.! A.target "_blank"
      H.! A.rel    "noopener noreferrer"
      $ H.toHtml (linkLabel lnk)

renderSection :: Section -> H.Html
renderSection sec =
  H.div H.! A.class_ "dt-section" $ do
    H.span H.! A.class_ "dt-section-title" $ H.toHtml (sectionTitle sec)
    H.span H.! A.class_ "dt-section-links" $ do
      mapM_ linkBadge (sectionLinks sec)
      statusBadge (sectionStatus sec)

------------------------------------------------------------------------
-- Work entry. `byline` is the author's name, given only in the Planned list.
------------------------------------------------------------------------

workMeta :: Work -> String
workMeta w = case workSource w of
  Just s | not (null s) -> s
  _ -> case (workYear w, fromMaybe "" (workVenue w)) of
         ("", "") -> ""
         ("", v)  -> v
         (y,  "") -> y
         (y,  v)  -> y ++ ". " ++ v

renderWork :: Maybe String -> String -> Work -> H.Html
renderWork byline aid w =
  H.div H.! A.class_ "dt-work"
        H.! A.id (H.toValue $ aid ++ "-" ++ workId w) $ do
    H.div H.! A.class_ "dt-work-title" $ H.toHtml (workTitle w)
    case workTitleEn w of
      Just t | not (null t) -> H.div H.! A.class_ "dt-work-meta" $ H.em (H.toHtml t)
      _ -> return ()
    let meta = case byline of
                 Just n  -> n ++ (if null (workMeta w) then "" else ". " ++ workMeta w)
                 Nothing -> workMeta w
    unless (null meta) $ H.div H.! A.class_ "dt-work-meta" $ H.toHtml meta
    case workNote w of
      Just n  -> H.div H.! A.class_ "dt-work-note" $ H.toHtml n
      Nothing -> return ()
    H.div H.! A.class_ "dt-sections" $ mapM_ renderSection (workSections w)

renderAuthor :: Author -> H.Html
renderAuthor a =
  H.section H.! A.class_ "dt-author" H.! A.id (H.toValue $ authorId a) $ do
    H.h2 H.! A.class_ "dt-author-heading" $ do
      H.toHtml (authorName a)
      H.span H.! A.class_ "dt-author-dates" $ H.toHtml (" (" ++ authorDates a ++ ")")
    H.p H.! A.class_ "dt-author-bio" $ H.toHtml (authorBio a)
    mapM_ (renderWork Nothing (authorId a)) (current a)

------------------------------------------------------------------------
-- Top-level generator
------------------------------------------------------------------------

generateRelativityTextsHTML :: Catalog -> String
generateRelativityTextsHTML catalog = R.renderHtml $
  H.div H.! A.class_ "dt-catalog" $ do
    H.p H.! A.class_ "dt-intro" $ do
      "Transcriptions and English translations of early papers on the theory of "
      "relativity, beginning with the debate of 1909–1911 over what a rigid body "
      "can be in special relativity. Each transcription follows the printed text, "
      "with the journal's page numbers in the margin and the printed equation "
      "numbers. Scans are from the "
      H.a H.! A.href "https://archive.org/" H.! A.target "_blank" $ "Internet Archive"
      "; LaTeX sources and PDF files are on "
      H.a H.! A.href "https://github.com/hhalvors/relativity-texts"
          H.! A.target "_blank" $ "GitHub"
      "."
    H.div H.! A.class_ "dt-legend" $ do
      H.span H.! A.class_ "dt-legend-label" $ "Status: "
      statusBadge "complete"
      statusBadge "in-progress"
      statusBadge "to-do"
    mapM_ renderAuthor [ a | a <- authors catalog, not (null (current a)) ]
    let later = [ (a, w) | a <- authors catalog, w <- works a, published w, planned w ]
    unless (null later) $
      H.section H.! A.class_ "dt-author dt-references" H.! A.id "planned" $ do
        H.h2 H.! A.class_ "dt-author-heading" $ "Planned"
        H.p H.! A.class_ "dt-author-bio" $
          "Related papers, to be transcribed and translated later."
        forM_ later $ \(a, w) -> renderWork (Just (authorName a)) (authorId a) w
