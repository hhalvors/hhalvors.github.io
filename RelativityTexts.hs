{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

-- | RelativityTexts.hs
-- Generates the catalog page /relativity/ for the Relativity Texts collection
-- (transcriptions and translations of early papers on relativity theory).
-- Data is read from data/relativity-texts.yaml, a symlink to
-- ~/relativity-texts/catalog.yaml, whose header documents the fields.
--
-- The collection is arranged by TOPIC, not by author (Hans, 2026-10-20): each
-- topic is a section, its papers listed in catalog order (chronological), the
-- papers of phase 2 under a "Planned" heading within the topic. A paper with
-- `publish: false` is left off the page. A paper with `status: in-copyright`
-- is listed with a link to an online copy and an "in copyright" badge. Uses the dt-* classes of the Danish
-- Texts page (css/style.css), so it needs no CSS of its own.
-- ~/relativity-texts/validate-catalog.py checks the file against these types.

module RelativityTexts (generateRelativityTextsHTML) where

import GHC.Generics               (Generic)
import Control.Monad               (unless)
import Data.Aeson                  (FromJSON(..), Options, genericParseJSON,
                                    defaultOptions, fieldLabelModifier)
import Data.Char                   (toLower, isUpper)
import Data.List                   (isPrefixOf)
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.String as R

------------------------------------------------------------------------
-- Data types
------------------------------------------------------------------------

data Catalog = Catalog
  { topics :: [Topic]
  } deriving (Generic, Show)

data Topic = Topic
  { topicId     :: String
  , topicTitle  :: String
  , topicNote   :: Maybe String
  , topicPapers :: [Paper]
  } deriving (Generic, Show)

data Paper = Paper
  { paperId      :: String
  , paperAuthor  :: String
  , paperDates   :: Maybe String
  , paperTitle   :: String
  , paperTitleEn :: Maybe String
  , paperYear    :: String
  , paperSource  :: Maybe String
  , paperNote    :: Maybe String
  , paperStatus  :: String
  , paperLinks   :: [Link]
  , paperPublish :: Maybe Bool
  , paperPhase   :: Maybe Int
  } deriving (Generic, Show)

data Link = Link
  { linkLabel :: String
  , linkUrl   :: String
  } deriving (Generic, Show)

------------------------------------------------------------------------
-- YAML field names: strip the type prefix, then camelCase -> kebab-case
-- (paperTitleEn -> title-en), as in DanishTexts.hs.
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

instance FromJSON Topic where
  parseJSON = genericParseJSON (opts "topic")

instance FromJSON Paper where
  parseJSON = genericParseJSON (opts "paper")

instance FromJSON Link where
  parseJSON = genericParseJSON (opts "link")

------------------------------------------------------------------------
-- Which papers are shown, and where
------------------------------------------------------------------------

published, planned :: Paper -> Bool
published p = paperPublish p /= Just False
planned   p = paperPhase p == Just 2

------------------------------------------------------------------------
-- Badges and links (same classes as the Danish Texts page)
------------------------------------------------------------------------

statusClass :: String -> String
statusClass "complete"    = "dt-status dt-status-complete"
statusClass "in-progress" = "dt-status dt-status-inprogress"
statusClass "in-copyright" = "dt-status dt-status-reference"
statusClass _             = "dt-status dt-status-todo"

statusLabel :: String -> String
statusLabel "complete"    = "complete"
statusLabel "in-progress" = "in progress"
statusLabel "to-do"       = "to do"
statusLabel "in-copyright" = "in copyright"
statusLabel s             = s

statusBadge :: String -> H.Html
statusBadge s =
  H.span H.! A.class_ (H.toValue $ statusClass s) $ H.toHtml (statusLabel s)

linkBadge :: Link -> H.Html
linkBadge lnk =
  H.a H.! A.href   (H.toValue $ linkUrl lnk)
      H.! A.class_ "dt-link"
      H.! A.target "_blank"
      H.! A.rel    "noopener noreferrer"
      $ H.toHtml (linkLabel lnk)

------------------------------------------------------------------------
-- A paper: the original title first (it carries the focus), then the English
-- title, then one grey line "Author (dates). Citation", the note, and the
-- links and status badge.
------------------------------------------------------------------------

renderPaper :: String -> Paper -> H.Html
renderPaper tid p =
  H.div H.! A.class_ "dt-work"
        H.! A.id (H.toValue $ tid ++ "-" ++ paperId p) $ do
    H.div H.! A.class_ "dt-work-title" $ H.toHtml (paperTitle p)
    case paperTitleEn p of
      Just t | not (null t) -> H.div H.! A.class_ "dt-work-meta" $ H.em (H.toHtml t)
      _ -> return ()
    H.div H.! A.class_ "dt-work-meta" $ H.toHtml meta
    case paperNote p of
      Just n  -> H.div H.! A.class_ "dt-work-note" $ H.toHtml n
      Nothing -> return ()
    H.div H.! A.class_ "dt-sections" $
      H.div H.! A.class_ "dt-section" $
        H.span H.! A.class_ "dt-section-links" $ do
          mapM_ linkBadge (paperLinks p)
          statusBadge (paperStatus p)
  where
    author = paperAuthor p ++ maybe "" (\d -> " (" ++ d ++ ")") (paperDates p)
    meta   = case paperSource p of
               Just s | not (null s) -> author ++ ". " ++ s
               _                     -> author ++ ", " ++ paperYear p

renderTopic :: Topic -> H.Html
renderTopic t =
  H.section H.! A.class_ "dt-author" H.! A.id (H.toValue $ topicId t) $ do
    H.h2 H.! A.class_ "dt-author-heading" $ H.toHtml (topicTitle t)
    case topicNote t of
      Just n  -> H.p H.! A.class_ "dt-author-bio" $ H.toHtml n
      Nothing -> return ()
    mapM_ (renderPaper (topicId t)) now
    unless (null later) $ do
      H.h3 H.! A.class_ "dt-seclit-head"
           H.! A.id (H.toValue $ topicId t ++ "-planned") $ "Planned"
      mapM_ (renderPaper (topicId t)) later
  where
    shown = filter published (topicPapers t)
    now   = filter (not . planned) shown
    later = filter planned shown

------------------------------------------------------------------------
-- Top-level generator
------------------------------------------------------------------------

generateRelativityTextsHTML :: Catalog -> String
generateRelativityTextsHTML catalog = R.renderHtml $
  H.div H.! A.class_ "dt-catalog" $ do
    H.p H.! A.class_ "dt-intro" $ do
      "Transcriptions and English translations of early papers on the theory of "
      "relativity, arranged by topic. Each transcription follows the printed text, "
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
    mapM_ renderTopic (topics catalog)
