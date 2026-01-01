{-# LANGUAGE OverloadedStrings #-}

module BibTeXParser (parseBibTeX, generateHTML, transformEntry) where

import Text.BibTeX.Parse (file)
import Text.BibTeX.Entry (T(..))
import Text.Parsec (ParseError, eof, parse)
import Data.List (sortOn, groupBy)
import Data.Function (on)
import Data.Ord (Down(..))
import Data.Maybe (fromMaybe)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.String as R

-- Function to parse the BibTeX content
parseBibTeX :: String -> Either ParseError [T]
parseBibTeX = parse (file <* eof) ""

-- Function to remove brackets from around letters
removeBrackets :: String -> String
removeBrackets [] = []
removeBrackets ('{':x:'}':xs) = x : removeBrackets xs
removeBrackets (x:xs) = x : removeBrackets xs

-- Function to apply the transformation to all fields in a BibTeX entry
transformEntry :: T -> T
transformEntry (Cons entryType identifier fields) = 
    Cons entryType identifier (map (\(k, v) -> (k, removeBrackets v)) fields)

-- Helper functions to extract various fields from a BibTeX entry
entryYear :: T -> Int
entryYear (Cons _ _ fields) = maybe 0 read (lookup "year" fields)

entryTitle :: T -> String
entryTitle (Cons _ _ fields) = fromMaybe "No Title" (lookup "title" fields)

-- Helper function to extract the DOI from a BibTeX entry
entryDOI :: T -> Maybe String
entryDOI (Cons _ _ fields) = lookup "doi" fields

-- Helper function to extract the URL from a BibTeX entry
entryURL :: T -> Maybe String
entryURL (Cons _ _ fields) = lookup "url" fields

-- Helper function to extract the journal from a BibTeX entry
entryJournal :: T -> Maybe String
entryJournal (Cons _ _ fields) = lookup "journal" fields

-- Function to render an entry with a possible DOI or URL hyperlink
renderEntry :: T -> H.Html
renderEntry entry = H.li $ do
    let title = entryTitle entry
    let maybeDOI = entryDOI entry
    let maybeURL = entryURL entry
    let maybeJournal = entryJournal entry
    let journal = fromMaybe "" maybeJournal -- Get the journal or an empty string
    
    let renderJournal = if not (null journal)
                        then H.toMarkup (" " :: String) >> H.i (H.toHtml journal)
                        else H.toMarkup ("" :: String)

    case (maybeDOI, maybeURL) of
      (Just doi, _) -> H.span $ do
                           H.a H.! A.href (H.toValue $ "https://doi.org/" ++ doi) $ H.toHtml title
                           renderJournal
                           entryToHTML entry
      (Nothing, Just url) -> H.span $ do
                           H.a H.! A.href (H.toValue url) $ H.toHtml title
                           renderJournal
                           entryToHTML entry
      (Nothing, Nothing) -> H.span $ do
                           H.toHtml title
                           renderJournal
                           entryToHTML entry

-- Function to convert a BibTeX entry to an HTML string using blaze-html
entryToHTML :: T -> H.Html
entryToHTML (Cons entryType entryKey fields) = do
  H.span H.! A.class_ "buttonline" H.! A.style "margin-left: 10px;" $ do
    H.a H.! A.class_ "badge badge-light" 
        H.! A.role "button" 
        H.! H.customAttribute "data-toggle" "collapse" 
        H.! A.href (H.toValue $ "#" ++ entryKey) 
        H.! H.customAttribute "aria-expanded" "false" 
        H.! H.customAttribute "aria-controls" (H.toValue entryKey) 
        H.! A.style "vertical-align:text-bottom" $ "BibTeX"
  H.div H.! A.class_ "collapse" 
        H.! A.id (H.toValue entryKey) $ do
    H.div H.! A.class_ "card card-body" $ do
      H.pre $ H.code $ H.toHtml $ bibtexEntryString (Cons entryType entryKey fields)
  where
    isOmittedKey (key, _) = key == "abstract"
    bibtexEntryString (Cons entryType entryKey fields) =
      "@" ++ entryType ++ "{" ++ entryKey ++ ",\n" ++
      concatMap fieldToString (filter (not . isOmittedKey) fields) ++
      "}\n"
    fieldToString (key, value) = "  " ++ key ++ " = " ++ "{ " ++ value ++ " },\n"                           

-- Function to generate HTML from BibTeX entries
generateHTML :: [T] -> String
generateHTML entries = R.renderHtml $ H.docTypeHtml $ do
    H.head $ do
        H.title "Publications"
        H.meta H.! A.name "viewport" H.! A.content "width=device-width, initial-scale=1.0"
        -- H.link H.! A.rel "stylesheet" H.! A.href "https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/css/bootstrap.min.css"
    H.body $ do
        mapM_ renderYearGroup sortedGroupedEntries
        H.script H.! A.src "https://code.jquery.com/jquery-3.5.1.slim.min.js" $ ""
        H.script H.! A.src "https://cdn.jsdelivr.net/npm/@popperjs/core@2.9.2/dist/umd/popper.min.js" $ ""
        H.script H.! A.src "https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/js/bootstrap.min.js" $ ""
  where
    sortedEntries = sortOn (Down . entryYear) entries
    groupedEntries = groupBy ((==) `on` entryYear) sortedEntries
    sortedGroupedEntries = map (\es -> (entryYear (head es), es)) groupedEntries

    renderYearGroup (year, entries) = do
        H.h2 (H.toHtml $ show year)
        H.ul $ mapM_ renderEntry entries

