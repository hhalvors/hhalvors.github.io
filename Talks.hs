{-# LANGUAGE OverloadedStrings #-}

-- | Talks.hs
-- Generates the HTML page for the Talks (slide decks) collection from the
-- unified catalog (data/talks-master.yaml, see TalksMaster). An "Upcoming"
-- section lists every future talk, with or without slides. Below it, past
-- talks are shown only if they have `web: true` and at least one link,
-- grouped by year, newest first.

module Talks (generateTalksHTML, generateHomeTalksHTML) where

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

-- | A recording is a different kind of thing from a slide deck, so it gets its
-- own colour and a visitor can scan the page for them.
linkBadge :: MLink -> H.Html
linkBadge lnk =
  H.a H.! A.href   (H.toValue $ lUrl lnk)
      H.! A.class_ (H.toValue cls)
      $ H.toHtml (lLabel lnk)
  where
    cls | lLabel lnk `elem` ["video", "audio"] = "talk-link talk-link-video"
        | otherwise                            = "talk-link" :: String

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
    -- One line on what the talk argues. Worth having wherever a recording is
    -- linked: it is what lets someone decide whether to spend the hour.
    case tNote t of
      Just n | not (null n) -> H.div H.! A.class_ "talk-note" $ renderMd n
      _                     -> return ()
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

-- | Is the talk in or after the build month `today` (year, month)? The
-- catalog records months, not days, so a talk in the current month counts as
-- upcoming. A year-only date ("2025") has month 0, so it is upcoming only if
-- its year lies in the future. Shared by the talks page and the home page.
isUpcoming :: (Int, Int) -> Int -> MTalk -> Bool
isUpcoming today year t = (year, monthOf t) >= today

-- | Past talks with slides online, by year. Upcoming talks are left out
-- here, since they are listed in their own section.
visibleGroups :: (Int, Int) -> MasterData -> [MYearGroup]
visibleGroups today d =
  [ g { myItems = sortBy (comparing (Down . monthOf)) vis }
  | g <- mdTalks d
  , let vis = [ t | t <- myItems g, isWeb t
                  , not (isUpcoming today (myYear g) t) ]
  , not (null vis) ]

-- | Every upcoming talk on the CV, soonest first, slides or not.
upcomingTalks :: (Int, Int) -> MasterData -> [MTalk]
upcomingTalks today d =
  map snd (sortBy (comparing fst)
             [ ((myYear g, monthOf t), t)
             | g <- mdTalks d
             , t <- myItems g
             , tCv t
             , isUpcoming today (myYear g) t ])

renderYear :: MYearGroup -> H.Html
renderYear g =
  H.section H.! A.class_ "talk-year" $ do
    H.h2 H.! A.class_ "talk-year-heading" $ H.toHtml (show (myYear g))
    mapM_ renderTalk (myItems g)

------------------------------------------------------------------------
-- Top-level generator
------------------------------------------------------------------------

-- | `today` is the build month as (year, month); see `buildMonthFile` in
-- site.hs.
generateTalksHTML :: (Int, Int) -> MasterData -> String
generateTalksHTML today d = R.renderHtml $
  H.div H.! A.class_ "talk-page" $ do
    H.div H.! A.class_ "talk-intro" $ renderMd (mdIntro d)
    case upcomingTalks today d of
      [] -> return ()
      ts -> H.section H.! A.class_ "talk-year" $ do
              H.h2 H.! A.class_ "talk-year-heading" $ "Upcoming"
              mapM_ renderTalk ts
    mapM_ renderYear (sortOn (Down . myYear) (visibleGroups today d))

------------------------------------------------------------------------
-- Upcoming and recent talks, for the home page.
--
-- Deliberately a weaker filter than the talks page uses: `isWeb` requires a
-- slide deck to exist, but a talk given last month (or next month) is worth
-- showing whether or not the deck is online. The year comes from the
-- enclosing group, so it is carried alongside each talk for sorting.
--
-- `today` is the build month as (year, month); see `isUpcoming`.
--
-- Returns (upcoming, recent): every upcoming talk, soonest first, and the n
-- most recent past talks, newest first. A part with no talks is "", which
-- lets the home page leave out its heading.
------------------------------------------------------------------------

generateHomeTalksHTML :: (Int, Int) -> Int -> MasterData -> (String, String)
generateHomeTalksHTML today n d = (render (upcomingTalks today d), render recent)
  where
    recent = take n (map snd (sortBy (comparing (Down . fst))
               [ ((myYear g, monthOf t), t)
               | g <- mdTalks d
               , t <- myItems g
               , tCv t
               , not (isUpcoming today (myYear g) t) ]))
    render [] = ""
    render ts = R.renderHtml $
      H.div H.! A.class_ "talk-page recent-talks" $ mapM_ renderTalk ts
