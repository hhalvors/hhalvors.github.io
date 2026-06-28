{-# LANGUAGE OverloadedStrings #-}

-- | TalksMaster.hs
-- The unified talk catalog (data/talks-master.yaml) is the single source of
-- truth for every talk. This module defines its schema and renders the CV's
-- talk sections from it (build-cv). See docs/talks-pipeline-plan.md.

module TalksMaster
  ( MasterData(..)
  , MYearGroup(..)
  , MTalk(..)
  , MLink(..)
  , renderInvited
  , renderOutreach
  , allTalks
  ) where

import Data.Aeson         (FromJSON(..), withObject, (.:), (.:?), (.!=))
import Data.Char          (isSpace)
import Data.List          (dropWhileEnd, intercalate, nub, sort, sortBy)
import Data.Ord           (Down(..), comparing)
import Data.Maybe         (fromMaybe)
import Control.Applicative ((<|>))

------------------------------------------------------------------------
-- Schema
------------------------------------------------------------------------

data MasterData = MasterData
  { mdIntro :: String
  , mdTalks :: [MYearGroup]
  } deriving Show

data MYearGroup = MYearGroup
  { myYear  :: Int
  , myItems :: [MTalk]
  } deriving Show

data MTalk = MTalk
  { tId             :: String
  , tTitle          :: Maybe String   -- delivered title
  , tAnnouncedTitle :: Maybe String   -- title given to organizers, if different
  , tKind           :: String         -- talk | conference | colloquium | ...
  , tEvent          :: Maybe String
  , tLocation       :: Maybe String
  , tDate           :: Maybe String    -- "YYYY" or "YYYY-MM"
  , tCoauthors      :: [String]
  , tSeriesCount    :: Maybe Int
  , tCv             :: Bool            -- include in the CV?
  , tWeb            :: Bool            -- show on the slides page?
  , tLinks          :: [MLink]
  , tNote           :: Maybe String
  , tCvRaw          :: Maybe String    -- verbatim CV LaTeX (ground truth)
  } deriving Show

data MLink = MLink
  { lLabel :: String
  , lUrl   :: String
  } deriving Show

------------------------------------------------------------------------
-- YAML parsing
------------------------------------------------------------------------

instance FromJSON MasterData where
  parseJSON = withObject "MasterData" $ \o ->
    MasterData <$> o .:? "intro" .!= ""
               <*> o .:  "talks"

instance FromJSON MYearGroup where
  parseJSON = withObject "MYearGroup" $ \o ->
    MYearGroup <$> o .: "year"
               <*> o .: "items"

instance FromJSON MTalk where
  parseJSON = withObject "MTalk" $ \o ->
    MTalk <$> o .:  "id"
          <*> o .:? "title"
          <*> o .:? "announced_title"
          <*> o .:? "kind"          .!= "talk"
          <*> o .:? "event"
          <*> o .:? "location"
          <*> o .:? "date"
          <*> o .:? "coauthors"     .!= []
          <*> o .:? "series_count"
          <*> o .:? "cv"            .!= True
          <*> o .:? "web"           .!= False
          <*> o .:? "links"         .!= []
          <*> o .:? "note"
          <*> o .:? "cv_raw"

instance FromJSON MLink where
  parseJSON = withObject "MLink" $ \o ->
    MLink <$> o .: "label" <*> o .: "url"

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

allTalks :: MasterData -> [(Int, MTalk)]
allTalks md = [ (myYear g, t) | g <- mdTalks md, t <- myItems g ]

strip :: String -> String
strip = dropWhileEnd isSpace . dropWhile isSpace

escapeLatex :: String -> String
escapeLatex = concatMap esc
  where
    esc '&' = "\\&"
    esc '%' = "\\%"
    esc '#' = "\\#"
    esc '_' = "\\_"
    esc c   = [c]

months :: [String]
months = ["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"]

-- | "2025-05" -> "May 2025"; "2013" -> "2013".
renderDate :: Maybe String -> String
renderDate Nothing  = ""
renderDate (Just s) = case break (== '-') s of
  (y, '-':mm) -> case reads mm :: [(Int,String)] of
                   [(m, _)] | m >= 1 && m <= 12 -> months !! (m-1) ++ " " ++ y
                   _ -> s
  _ -> s

-- | Body of one CV \item. Prefer the verbatim cv_raw; otherwise synthesise
-- from the structured fields (used for talks added after the migration).
itemBody :: MTalk -> String
itemBody t = case tCvRaw t of
  Just raw | not (null (strip raw)) -> strip raw
  _ -> synth
  where
    ttl   = fromMaybe "" (tAnnouncedTitle t <|> tTitle t)
    title = "``" ++ escapeLatex ttl ++ "''"
    co    = case tCoauthors t of
              [] -> ""
              cs -> " (with " ++ intercalate " and " (map escapeLatex cs) ++ ")"
    meta  = intercalate ", " $ filter (not . null)
              [ maybe "" escapeLatex (tEvent t)
              , maybe "" escapeLatex (tLocation t)
              , renderDate (tDate t) ]
    synth = title ++ co ++ (if null meta then "" else " " ++ meta) ++ "."

------------------------------------------------------------------------
-- Renderers (build-cv). Each emits the \item lines only; cv.tex keeps the
-- surrounding \begin{list}/\begin{itemize} wrapper and \input{}s these.
------------------------------------------------------------------------

-- | Month (1–12) parsed from a "YYYY-MM" date; 0 if absent. Used only to
-- sort within a year (newest first); never printed.
monthOf :: MTalk -> Int
monthOf t = case tDate t of
  Just s -> case break (== '-') s of
              (_, '-':mm) -> case reads mm :: [(Int, String)] of
                               [(m, _)] -> m
                               _        -> 0
              _ -> 0
  Nothing -> 0

-- | One invited-talk \item, built from structured fields. NO date is printed
-- (the year appears once per group via \yearlabel). Co-authors render as
-- "(with ...)"; event and location follow the (announced) title.
talkBody :: MTalk -> String
talkBody t = unwords (filter (not . null) [titlePart, co, meta])
  where
    ttl       = fromMaybe "" (tAnnouncedTitle t <|> tTitle t)
    titlePart = if null ttl then "" else "``" ++ escapeLatex ttl ++ "''"
    co        = case tCoauthors t of
                  [] -> ""
                  cs -> "(with " ++ intercalate " and " (map escapeLatex cs) ++ ")"
    meta      = intercalate ", " $ filter (not . null)
                  [ maybe "" escapeLatex (tEvent t)
                  , maybe "" escapeLatex (tLocation t) ]

talkLine :: Bool -> Int -> MTalk -> String
talkLine isFirst yr t =
  "\\item " ++ (if isFirst then "\\yearlabel{" ++ show yr ++ "} " else "")
            ++ talkBody t

-- | Invited lectures and conference presentations. Years newest-first; within
-- a year, talks are ordered by month (newest first). \yearlabel marks each
-- year's first item.
renderInvited :: MasterData -> String
renderInvited md = intercalate "\n\n" (concatMap block years) ++ "\n"
  where
    items     = [ (myYear g, t) | g <- mdTalks md, t <- myItems g
                                , tCv t, tKind t /= "outreach" ]
    years     = reverse (sort (nub (map fst items)))
    perYear y = sortBy (comparing (Down . monthOf)) [ t | (yy, t) <- items, yy == y ]
    block y   = [ talkLine (i == 0) y t | (i, t) <- zip [0 :: Int ..] (perYear y) ]

-- | Outreach activities, document order, no year labels.
renderOutreach :: MasterData -> String
renderOutreach md = intercalate "\n\n" (map ("\\item " ++) bodies) ++ "\n"
  where
    bodies = [ itemBody t | (_, t) <- allTalks md, tCv t, tKind t == "outreach" ]
