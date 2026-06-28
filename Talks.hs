{-# LANGUAGE OverloadedStrings #-}

-- | Talks.hs
-- Generates the HTML page for the Talks (slide decks) collection from the
-- unified catalog (data/talks-master.yaml, see TalksMaster). Only talks with
-- `web: true` and at least one link are shown, grouped by year, newest first.

module Talks (generateTalksHTML) where

import Data.List                   (intercalate, sortOn, sortBy)
import Data.Maybe                  (catMaybes, fromMaybe)
import Data.Ord                    (Down(..), comparing)
import qualified Data.Text                   as T
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html             (preEscapedToHtml)
import qualified Text.Blaze.Html.Renderer.String as R
import           Data.Default                (def)
import           Text.Pandoc                 (pandocExtensions, readMarkdown,
                                              runPure, writeHtml5String)
import           Text.Pandoc.Options         (ReaderOptions(readerExtensions))

import           TalksMaster                 (MasterData(..), MYearGroup(..),
                                              MTalk(..), MLink(..))

------------------------------------------------------------------------
-- Markdown -> HTML (pure, via Pandoc).
------------------------------------------------------------------------

mdToHtmlString :: String -> String
mdToHtmlString s =
  case runPure (readMarkdown ropts (T.pack s) >>= writeHtml5String def) of
    Left _  -> s
    Right t -> T.unpack t
  where
    ropts = def { readerExtensions = pandocExtensions }

renderMd :: String -> H.Html
renderMd = preEscapedToHtml . mdToHtmlString

------------------------------------------------------------------------
-- Date formatting: "2026-06" -> "June 2026"; "2025" -> "2025".
------------------------------------------------------------------------

monthsLong :: [String]
monthsLong =
  [ "January","February","March","April","May","June"
  , "July","August","September","October","November","December" ]

fmtDate :: String -> String
fmtDate s = case break (== '-') s of
  (y, '-':mm) -> case reads mm :: [(Int, String)] of
                   [(m, _)] | m >= 1 && m <= 12 -> monthsLong !! (m-1) ++ " " ++ y
                   _ -> s
  _ -> s

------------------------------------------------------------------------
-- Light LaTeX -> text cleanup for HTML display (titles/event/location).
-- The CV keeps the raw LaTeX; only the web page needs this.
------------------------------------------------------------------------

cleanText :: String -> String
cleanText s = T.unpack
  $ T.replace "$" ""        -- drop math delimiters: $2$ -> 2, $C^*$ -> C*
  $ T.replace "^" ""        -- C^* -> C*
  $ T.replace "--" "\x2013" -- en-dash
  $ T.replace "---" "\x2014" -- em-dash (applied before "--")
  $ T.pack s

------------------------------------------------------------------------
-- Link badge
------------------------------------------------------------------------

linkBadge :: MLink -> H.Html
linkBadge lnk =
  H.a H.! A.href   (H.toValue $ lUrl lnk)
      H.! A.class_ "talk-link"
      $ H.toHtml (lLabel lnk)

------------------------------------------------------------------------
-- A single talk
------------------------------------------------------------------------

renderTalk :: MTalk -> H.Html
renderTalk t =
  H.div H.! A.class_ "talk-item" $ do
    H.div H.! A.class_ "talk-title" $ H.toHtml (cleanText (fromMaybe "" (tTitle t)))
    let meta = filter (not . null)
                 (catMaybes [ fmap cleanText (tEvent t)
                            , fmap cleanText (tLocation t)
                            , fmap fmtDate (tDate t) ])
    if null meta
      then return ()
      else H.div H.! A.class_ "talk-meta"
                 $ H.toHtml (intercalate " · " meta)
    H.div H.! A.class_ "talk-links" $
      mapM_ linkBadge (tLinks t)

------------------------------------------------------------------------
-- A year group (only web-visible talks)
------------------------------------------------------------------------

isWeb :: MTalk -> Bool
isWeb t = tWeb t && not (null (tLinks t))

-- | Month (1–12) from a "YYYY-MM" date; 0 if absent. Used to sort within a year.
monthOf :: MTalk -> Int
monthOf t = case tDate t of
  Just s -> case break (== '-') s of
              (_, '-':mm) -> case reads mm :: [(Int, String)] of
                               [(m, _)] -> m
                               _        -> 0
              _ -> 0
  Nothing -> 0

visibleGroups :: MasterData -> [MYearGroup]
visibleGroups d =
  [ g { myItems = sortBy (comparing (Down . monthOf)) vis }
  | g <- mdTalks d
  , let vis = filter isWeb (myItems g)
  , not (null vis) ]

renderYear :: MYearGroup -> H.Html
renderYear g =
  H.section H.! A.class_ "talk-year" $ do
    H.h2 H.! A.class_ "talk-year-heading" $ H.toHtml (show (myYear g))
    mapM_ renderTalk (myItems g)

------------------------------------------------------------------------
-- Top-level generator
------------------------------------------------------------------------

generateTalksHTML :: MasterData -> String
generateTalksHTML d = R.renderHtml $
  H.div H.! A.class_ "talk-page" $ do
    H.div H.! A.class_ "talk-intro" $ renderMd (mdIntro d)
    mapM_ renderYear (sortOn (Down . myYear) (visibleGroups d))
