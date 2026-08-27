{-# LANGUAGE OverloadedStrings #-}

module CoursePages.Schedule
  ( Link(..)
  , DocRef(..)
  , ScheduleItem(..)
  , SchedulePage(..)
  , loadScheduleYaml
  , scheduleItemCtx
  , compileSchedulePage
  ) where

import Hakyll
import Data.Aeson (FromJSON(..), withObject, (.:), (.:?))
import Data.Yaml (decodeFileEither)
import Data.Maybe (fromMaybe)

-- A button/link in the card
data Link = Link
  { label :: String
  , href  :: String
  } deriving Show

instance FromJSON Link where
  parseJSON = withObject "Link" $ \o ->
    Link <$> o .: "label"
         <*> o .: "href"

-- A primary “lecture notes” / “handout” link (optional)
data DocRef = DocRef
  { docLabel :: String
  , docHref  :: String
  } deriving Show

instance FromJSON DocRef where
  parseJSON = withObject "DocRef" $ \o ->
    DocRef <$> o .: "label"
          <*> o .: "href"

-- One card in the grid (week, precept, exam, etc.)
data ScheduleItem = ScheduleItem
  { kind       :: String          -- "week", "exam", "precept", ...
  , number     :: Maybe Int       -- week/precept number etc.
  , dateLabel  :: String
  , itemTitle  :: String
  , reading    :: Maybe String
  , info       :: Maybe String
  , primary    :: Maybe DocRef
  , materials  :: Maybe [Link]
  } deriving Show

instance FromJSON ScheduleItem where
  parseJSON = withObject "ScheduleItem" $ \o ->
    ScheduleItem <$> o .:  "kind"
                 <*> o .:? "number"
                 <*> o .:  "dateLabel"
                 <*> o .:  "title"
                 <*> o .:? "reading"
                 <*> o .:? "info"
                 <*> o .:? "primary"
                 <*> o .:? "materials"

-- The whole page
data SchedulePage = SchedulePage
  { pageTitle :: String
  , note      :: Maybe String
  , items     :: [ScheduleItem]
  } deriving Show

instance FromJSON SchedulePage where
  parseJSON = withObject "SchedulePage" $ \o ->
    SchedulePage <$> o .:  "title"
                 <*> o .:? "note"
                 <*> o .:  "items"

loadScheduleYaml :: FilePath -> Compiler SchedulePage
loadScheduleYaml fp = unsafeCompiler $ do
  res <- decodeFileEither fp
  case res of
    Left err -> fail ("YAML decode error in " <> fp <> ": " <> show err)
    Right x  -> pure x

linkCtx :: Context Link
linkCtx =
     field "label" (pure . label . itemBody)
  <> (field "href"  (pure . href  . itemBody))

scheduleItemCtx :: Context ScheduleItem
scheduleItemCtx =
     field "kind"        (pure . kind . itemBody)

  <> field "isWeek"      (pure . (\b -> if kind b == "week" then "true" else "") . itemBody)
  <> field "isExam"      (pure . (\b -> if kind b == "exam" then "true" else "") . itemBody)

  <> field "hasNumber"   (pure . maybe "" (const "true") . number . itemBody)
  <> field "number"      (pure . maybe "" show . number . itemBody)

  <> field "dateLabel"   (pure . dateLabel . itemBody)
  <> field "title"       (pure . itemTitle . itemBody)

  <> field "hasReading"  (pure . maybe "" (const "true") . reading . itemBody)
  <> field "reading"     (pure . fromMaybe "" . reading . itemBody)

  <> field "hasInfo"     (pure . maybe "" (const "true") . info . itemBody)
  <> field "info"        (pure . fromMaybe "" . info . itemBody)

  <> field "hasPrimary"   (pure . maybe "" (const "true") . primary . itemBody)
  <> field "primaryLabel" (pure . maybe "" docLabel . primary . itemBody)
  <> field "primaryHref"  (pure . maybe "" docHref  . primary . itemBody)

  <> field "hasMaterials"
       (pure . (\b -> if maybe False (not . null) (materials b) then "true" else "") . itemBody)

  <> listFieldWith "materials" linkCtx
       (\it -> mapM makeItem (fromMaybe [] (materials (itemBody it))))  



-- Given YAML + template, produce the Item String for the page body
compileSchedulePage
  :: FilePath            -- ^ YAML file
  -> Identifier          -- ^ template identifier
  -> Context String      -- ^ extra context (siteCtx etc.)
  -> Compiler (Item String)
compileSchedulePage yamlPath templateId extraCtx = do
  sp <- loadScheduleYaml yamlPath
  itemItems <- mapM makeItem (items sp)
  let ctx = mconcat
        [ constField "title" (pageTitle sp)
        , constField "note"  (fromMaybe "" (note sp))
        , listField "items" scheduleItemCtx (pure itemItems)
        , defaultContext
        , extraCtx
        ]
  makeItem ("" :: String) >>= loadAndApplyTemplate templateId ctx
  
