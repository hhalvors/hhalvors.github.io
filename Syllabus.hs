{-# LANGUAGE OverloadedStrings #-}

-- | Syllabus.hs
-- Generates the HTML body for a course syllabus / reading-schedule page.
-- Data is read from a per-course YAML file (e.g. courses/phi367_s2026/syllabus.yaml).
--
-- Modelled on DanishTexts.hs: explicit FromJSON instances + a Blaze-HTML
-- renderer, with prose fields authored as Markdown and rendered to HTML at
-- build time via a pure Pandoc helper.

module Syllabus (generateSyllabusHTML, Syllabus) where

import           Data.Aeson                  (FromJSON(..), withObject,
                                              (.:), (.:?), (.!=))
import           Data.Char                   (isSpace)
import           Data.List                   (dropWhileEnd, intersperse,
                                              isPrefixOf, isSuffixOf)
import           Data.Maybe                  (catMaybes, fromMaybe)
import qualified Data.Text                   as T
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html             (preEscapedToHtml)
import qualified Text.Blaze.Html.Renderer.String as R
import           Data.Default                (def)
import           Text.Pandoc                 (pandocExtensions, readMarkdown,
                                              runPure, writeHtml5String)
import           Text.Pandoc.Options         (ReaderOptions(readerExtensions))

------------------------------------------------------------------------
-- Data types
------------------------------------------------------------------------

data Syllabus = Syllabus
  { sylCourse   :: CourseMeta
  , sylIntro    :: String
  , sylWeeks    :: [Week]
  , sylSuppIntro :: Maybe String
  , sylSupp     :: [Book]
  , sylColophon :: Maybe String
  , sylOther    :: Maybe String
  } deriving Show

data CourseMeta = CourseMeta
  { cmTitle      :: String
  , cmSubtitle   :: Maybe String
  , cmDates      :: Maybe String
  , cmLocation   :: Maybe String
  , cmTextbook   :: Maybe String
  , cmUniversity :: Maybe String
  } deriving Show

data Week = Week
  { wkNumber :: Int
  , wkTheme  :: String
  , wkDays   :: [Day]
  } deriving Show

data Day = Day
  { dyWeekday      :: String
  , dyDate         :: String
  , dyTitle        :: Maybe String
  , dyKind         :: String          -- "session" (default), "trip", "optional-trip", "off"
  , dyNote         :: Maybe String
  , dyReadingLabel :: Maybe String
  , dyReading      :: [Reading]
  , dyBody         :: Maybe String
  , dyAfternoon    :: Maybe Afternoon
  } deriving Show

data Reading = Reading
  { rdTitle :: String
  , rdPages :: Maybe String
  } deriving Show

data Afternoon = Afternoon
  { afLabel  :: Maybe String
  , afWhen   :: Maybe String          -- "Afternoon", "Evening", "Afternoon/Evening"
  , afFree   :: Bool
  , afGlance :: Maybe String          -- override text for the at-a-glance table
  , afBody   :: Maybe String
  } deriving Show

data Book = Book
  { bkAuthor      :: String
  , bkTitle       :: String
  , bkPublication :: Maybe String
  , bkNote        :: Maybe String
  } deriving Show

------------------------------------------------------------------------
-- FromJSON
------------------------------------------------------------------------

instance FromJSON Syllabus where
  parseJSON = withObject "Syllabus" $ \o ->
    Syllabus <$> o .:  "course"
             <*> o .:? "intro" .!= ""
             <*> o .:? "weeks" .!= []
             <*> o .:? "supplementary_intro"
             <*> o .:? "supplementary_reading" .!= []
             <*> o .:? "colophon"
             <*> o .:? "other_places"

instance FromJSON CourseMeta where
  parseJSON = withObject "course" $ \o ->
    CourseMeta <$> o .:  "title"
               <*> o .:? "subtitle"
               <*> o .:? "dates"
               <*> o .:? "location"
               <*> o .:? "textbook"
               <*> o .:? "university"

instance FromJSON Week where
  parseJSON = withObject "week" $ \o ->
    Week <$> o .:  "number"
         <*> o .:  "theme"
         <*> o .:? "days" .!= []

instance FromJSON Day where
  parseJSON = withObject "day" $ \o ->
    Day <$> o .:  "weekday"
        <*> o .:  "date"
        <*> o .:? "title"
        <*> o .:? "kind" .!= "session"
        <*> o .:? "note"
        <*> o .:? "reading_label"
        <*> o .:? "reading" .!= []
        <*> o .:? "body"
        <*> o .:? "afternoon"

instance FromJSON Reading where
  parseJSON = withObject "reading" $ \o ->
    Reading <$> o .:  "title"
            <*> o .:? "pages"

instance FromJSON Afternoon where
  parseJSON = withObject "afternoon" $ \o ->
    Afternoon <$> o .:? "label"
              <*> o .:? "when"
              <*> o .:? "free" .!= False
              <*> o .:? "glance"
              <*> o .:? "body"

instance FromJSON Book where
  parseJSON = withObject "book" $ \o ->
    Book <$> o .:  "author"
         <*> o .:  "title"
         <*> o .:? "publication"
         <*> o .:? "note"

------------------------------------------------------------------------
-- Markdown -> HTML (pure, via Pandoc)
------------------------------------------------------------------------

mdToHtmlString :: String -> String
mdToHtmlString s =
  case runPure (readMarkdown ropts (T.pack s) >>= writeHtml5String def) of
    Left _  -> s
    Right t -> T.unpack t
  where
    ropts = def { readerExtensions = pandocExtensions }

-- Block-level markdown: keep paragraph wrapping.
renderMd :: String -> H.Html
renderMd = preEscapedToHtml . mdToHtmlString

-- Inline markdown: strip a single enclosing <p>…</p> so it can sit inside a span.
renderMdInline :: String -> H.Html
renderMdInline = preEscapedToHtml . stripPara . mdToHtmlString

stripPara :: String -> String
stripPara raw =
  let s = dropWhileEnd isSpace raw
  in if "<p>" `isPrefixOf` s && "</p>" `isSuffixOf` s
       then take (length s - 7) (drop 3 s)   -- drop "<p>" (3) and "</p>" (4)
       else s

------------------------------------------------------------------------
-- Small helpers
------------------------------------------------------------------------

nonEmpty :: String -> Bool
nonEmpty = not . null

-- "June 24" -> "Jun 24", "July 3" -> "Jul 3"
abbrevDate :: String -> String
abbrevDate d =
  case words d of
    (m:rest) -> unwords (abbrevMonth m : rest)
    []       -> d
  where
    abbrevMonth "January"   = "Jan"
    abbrevMonth "February"  = "Feb"
    abbrevMonth "March"     = "Mar"
    abbrevMonth "April"     = "Apr"
    abbrevMonth "June"      = "Jun"
    abbrevMonth "July"      = "Jul"
    abbrevMonth "August"    = "Aug"
    abbrevMonth "September" = "Sep"
    abbrevMonth "October"   = "Oct"
    abbrevMonth "November"  = "Nov"
    abbrevMonth "December"  = "Dec"
    abbrevMonth other       = other

abbrevWeekday :: String -> String
abbrevWeekday = take 3

kindBadge :: String -> Maybe String
kindBadge "trip"          = Just "Trip"
kindBadge "optional-trip" = Just "Optional"
kindBadge "off"           = Just "No class"
kindBadge _               = Nothing

------------------------------------------------------------------------
-- Rendering: hero + intro
------------------------------------------------------------------------

renderHero :: CourseMeta -> H.Html
renderHero cm =
  H.header H.! A.class_ "syl-hero" $ do
    case cmUniversity cm of
      Just u  -> H.div H.! A.class_ "syl-hero-kicker" $ H.toHtml u
      Nothing -> return ()
    H.h1 H.! A.class_ "syl-hero-title" $ H.toHtml (cmTitle cm)
    case cmSubtitle cm of
      Just s  -> H.div H.! A.class_ "syl-hero-subtitle" $ H.toHtml s
      Nothing -> return ()
    let metaBits = catMaybes [cmDates cm, cmLocation cm]
    if null metaBits
      then return ()
      else H.div H.! A.class_ "syl-hero-meta" $ renderMeta metaBits
    case cmTextbook cm of
      Just tb -> H.div H.! A.class_ "syl-hero-textbook" $ do
                   "Readings follow "
                   H.em (H.toHtml tb)
      Nothing -> return ()

-- Render meta chunks separated by a middot.
renderMeta :: [String] -> H.Html
renderMeta xs = sequence_ (intersperse dot (map item xs))
  where
    item s = H.span H.! A.class_ "syl-meta-item" $ H.toHtml s
    dot     = H.span H.! A.class_ "syl-dot" $ "·"

renderIntro :: String -> H.Html
renderIntro s
  | nonEmpty s = H.section H.! A.class_ "syl-intro" $ renderMd s
  | otherwise  = return ()

------------------------------------------------------------------------
-- Rendering: weeks + days
------------------------------------------------------------------------

renderWeek :: Week -> H.Html
renderWeek w =
  H.section H.! A.class_ "syl-week"
            H.! A.id (H.toValue ("week-" ++ show (wkNumber w))) $ do
    H.div H.! A.class_ "syl-week-head" $ do
      H.span H.! A.class_ "syl-week-num" $ H.toHtml ("Week " ++ show (wkNumber w))
      H.h2 H.! A.class_ "syl-week-theme" $ H.toHtml (wkTheme w)
    H.div H.! A.class_ "syl-days" $
      mapM_ renderDay (wkDays w)

renderDay :: Day -> H.Html
renderDay d =
  H.article H.! A.class_ (H.toValue ("syl-day syl-day-" ++ dyKind d)) $ do
    H.div H.! A.class_ "syl-day-date" $ do
      H.span H.! A.class_ "syl-day-weekday" $ H.toHtml (dyWeekday d)
      H.span H.! A.class_ "syl-day-num"     $ H.toHtml (dyDate d)
    H.div H.! A.class_ "syl-day-main" $ do
      H.div H.! A.class_ "syl-day-titlerow" $ do
        case dyTitle d of
          Just t  -> H.h3 H.! A.class_ "syl-day-title" $ H.toHtml t
          Nothing -> return ()
        case kindBadge (dyKind d) of
          Just b  -> H.span H.! A.class_ (H.toValue ("syl-badge syl-badge-" ++ dyKind d))
                            $ H.toHtml b
          Nothing -> return ()
      case dyNote d of
        Just n  -> H.p H.! A.class_ "syl-day-note" $ renderMdInline n
        Nothing -> return ()
      renderReading d
      case dyBody d of
        Just b  -> H.div H.! A.class_ "syl-day-body" $ renderMd b
        Nothing -> return ()
      renderAfternoon (dyAfternoon d)

renderReading :: Day -> H.Html
renderReading d
  | null (dyReading d) = return ()
  | otherwise =
      H.div H.! A.class_ "syl-reading" $ do
        H.div H.! A.class_ "syl-reading-label" $
          H.toHtml (fromMaybe "Morning reading" (dyReadingLabel d))
        H.ul H.! A.class_ "syl-reading-list" $
          mapM_ renderReadingItem (dyReading d)

renderReadingItem :: Reading -> H.Html
renderReadingItem r =
  H.li H.! A.class_ "syl-reading-item" $ do
    H.span H.! A.class_ "syl-reading-title" $ H.toHtml (rdTitle r)
    case rdPages r of
      Just p  -> do
        " "
        H.span H.! A.class_ "syl-reading-pages" $ H.toHtml ("pp. " ++ p)
      Nothing -> return ()

renderAfternoon :: Maybe Afternoon -> H.Html
renderAfternoon Nothing = return ()
renderAfternoon (Just a)
  | afFree a =
      H.div H.! A.class_ "syl-afternoon syl-afternoon-free" $ do
        H.span H.! A.class_ "syl-free-label" $ "Free afternoon"
        case afBody a of
          Just b  -> do
            " "
            H.span H.! A.class_ "syl-free-note" $ renderMdInline b
          Nothing -> return ()
  | otherwise =
      H.div H.! A.class_ "syl-afternoon" $ do
        H.div H.! A.class_ "syl-afternoon-label" $ do
          H.span H.! A.class_ "syl-afternoon-when" $
            H.toHtml (fromMaybe "Afternoon" (afWhen a))
          H.toHtml (" — " ++ fromMaybe "" (afLabel a))
        case afBody a of
          Just b  -> H.div H.! A.class_ "syl-afternoon-body" $ renderMd b
          Nothing -> return ()

------------------------------------------------------------------------
-- Rendering: derived "Site Visits at a Glance" table
------------------------------------------------------------------------

data GlanceRow = GlanceRow
  { grWeek  :: Int
  , grDay   :: String
  , grVenue :: String
  }

glanceRows :: [Week] -> [GlanceRow]
glanceRows ws =
  [ row
  | w <- ws
  , d <- wkDays w
  , Just row <- [dayGlance (wkNumber w) d]
  ]

dayGlance :: Int -> Day -> Maybe GlanceRow
dayGlance wn d =
  case venue of
    Just v  -> Just (GlanceRow wn dayLabel v)
    Nothing -> Nothing
  where
    dayLabel = abbrevWeekday (dyWeekday d) ++ " " ++ abbrevDate (dyDate d)
    isTrip   = dyKind d `elem` ["trip", "optional-trip"]
    aft      = dyAfternoon d
    venue =
      case aft of
        Just a
          | Just g <- afGlance a -> Just g
          | Just l <- afLabel  a -> Just l
        _ | isTrip               -> dyTitle d
          | otherwise            -> Nothing

renderGlance :: [Week] -> H.Html
renderGlance ws =
  let rows = glanceRows ws
  in if null rows
       then return ()
       else H.section H.! A.class_ "syl-glance" $ do
              H.h2 H.! A.class_ "syl-h2" $ "Site Visits at a Glance"
              H.table H.! A.class_ "syl-glance-table" $ do
                H.thead $ H.tr $ do
                  H.th "Week"
                  H.th "Day"
                  H.th "Venue"
                H.tbody $ mapM_ renderGlanceRow rows

renderGlanceRow :: GlanceRow -> H.Html
renderGlanceRow r = H.tr $ do
  H.td H.! A.class_ "syl-glance-week"  $ H.toHtml (show (grWeek r))
  H.td H.! A.class_ "syl-glance-day"   $ H.toHtml (grDay r)
  H.td H.! A.class_ "syl-glance-venue" $ H.toHtml (grVenue r)

------------------------------------------------------------------------
-- Rendering: supplementary reading, colophon, other places
------------------------------------------------------------------------

renderSupp :: Maybe String -> [Book] -> H.Html
renderSupp _ [] = return ()
renderSupp mIntro bs =
  H.section H.! A.class_ "syl-supp" $ do
    H.h2 H.! A.class_ "syl-h2" $ "Supplementary Reading"
    case mIntro of
      Just i  -> H.p H.! A.class_ "syl-supp-intro" $ renderMdInline i
      Nothing -> return ()
    H.ul H.! A.class_ "syl-supp-list" $ mapM_ renderBook bs

renderBook :: Book -> H.Html
renderBook b =
  H.li H.! A.class_ "syl-supp-item" $ do
    H.span H.! A.class_ "syl-supp-author" $ H.toHtml (bkAuthor b ++ ", ")
    H.cite H.! A.class_ "syl-supp-title" $ H.toHtml (bkTitle b)
    case bkPublication b of
      Just p  -> do
        " "
        H.span H.! A.class_ "syl-supp-pub" $ H.toHtml ("(" ++ p ++ ")")
      Nothing -> return ()
    case bkNote b of
      Just n  -> H.span H.! A.class_ "syl-supp-note" $ do
                   " — "
                   renderMdInline n
      Nothing -> return ()

renderColophon :: Maybe String -> H.Html
renderColophon Nothing  = return ()
renderColophon (Just c) =
  H.div H.! A.class_ "syl-colophon" $ renderMd c

renderOther :: Maybe String -> H.Html
renderOther Nothing  = return ()
renderOther (Just o) =
  H.section H.! A.class_ "syl-other" $ do
    H.h2 H.! A.class_ "syl-h2" $ "Other Places Worth Your Time"
    H.div H.! A.class_ "syl-other-body" $ renderMd o

------------------------------------------------------------------------
-- Top-level
------------------------------------------------------------------------

generateSyllabusHTML :: Syllabus -> String
generateSyllabusHTML syl = R.renderHtml $
  H.div H.! A.class_ "syl" $ do
    renderHero    (sylCourse syl)
    renderIntro   (sylIntro syl)
    mapM_ renderWeek (sylWeeks syl)
    renderGlance  (sylWeeks syl)
    renderSupp    (sylSuppIntro syl) (sylSupp syl)
    renderColophon (sylColophon syl)
    renderOther   (sylOther syl)
