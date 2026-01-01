{-# LANGUAGE OverloadedStrings #-}

module PubList
  ( parseBibTeXFile
  , transformEntry
  , generateHtml
  ) where

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

-- Function to parse a BibTeX file and print the entries
parseBibTeXFile :: FilePath -> IO (Either ParseError [T])
parseBibTeXFile filePath = do
    content <- readFile filePath
    return (parseBibTeX content)

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

-- Function to check if --bib flag is present
hasBibFlag :: [String] -> Bool
hasBibFlag = elem "--bib"

entryPDF :: T -> Maybe String
entryPDF (Cons _ _ fields) = lookup "pdf" fields

entryDOI :: T -> Maybe String
entryDOI (Cons _ _ fields) = lookup "doi" fields

entryURL :: T -> Maybe String
entryURL (Cons _ _ fields) = lookup "url" fields

entryYear :: T -> Int
entryYear (Cons _ _ fields) = maybe 0 read (lookup "year" fields)

entryTitle :: T -> String
entryTitle (Cons _ _ fields) = fromMaybe "No Title" (lookup "title" fields)
        
-- Function to convert a BibTeX entry to an HTML list item, with optional BibTeX button
entryToHTML :: Bool -> T -> H.Html
entryToHTML showBib entry@(Cons entryType entryKey fields) = H.li $ do
    let title = entryTitle entry
    let maybeDOI = entryDOI entry
    let maybeURL = entryURL entry

    case (maybeURL, maybeDOI) of
        (Just url, _) -> H.a H.! A.href (H.toValue url) $ H.toHtml title
        (Nothing, Just doi) -> H.a H.! A.href (H.toValue $ "https://doi.org/" ++ doi) $ H.toHtml title
        (Nothing, Nothing) -> H.toHtml title
    
    H.span H.! A.class_ "buttonline" $ do
        -- Conditionally add a BibTeX button
        if showBib
        then H.a H.! A.class_ "badge badge-light" 
                H.! A.role "button" 
                H.! H.customAttribute "data-toggle" "collapse" 
                H.! A.href (H.toValue $ "#" ++ entryKey) 
                H.! H.customAttribute "aria-expanded" "false" 
                H.! H.customAttribute "aria-controls" (H.toValue entryKey) 
                H.! A.style "vertical-align:text-bottom" $ "BibTeX"
        else return ()
        -- Conditionally add a PDF button if a pdf field exists
        case entryPDF entry of
            Just pdfValue -> 
                H.a H.! A.class_ "badge badge-light"
                    H.! A.href (H.toValue $ "pdf/" ++ pdfValue ++ ".pdf")
                    H.! A.style "vertical-align:text-bottom; margin-left:5px;" $ "PDF"
            Nothing -> return ()
    -- BibTeX collapse section with code block
    if showBib
    then H.div H.! A.class_ "collapse" 
            H.! A.id (H.toValue entryKey) $ do
        H.div H.! A.class_ "card card-body" $ do
            H.pre $ H.code $ H.toHtml $ bibtexEntryString (Cons entryType entryKey fields)
    else return ()
  where
    isOmittedKey (key, _) = key == "abstract"
    bibtexEntryString (Cons entryType entryKey fields) =
      "@" ++ entryType ++ "{" ++ entryKey ++ ",\n" ++
      concatMap fieldToString (filter (not . isOmittedKey) fields) ++
      "}\n"
    fieldToString (key, value) = "  " ++ key ++ " = " ++ "{ " ++ value ++ " },\n"

-- | Generate HTML fragment for publications, grouped by year -----------------
generateHtml :: Bool      -- ^ showBib?
             -> [T]       -- ^ parsed BibTeX entries
             -> String    -- ^ HTML fragment (no <html>, <head>, <body>)
generateHtml showBib entries = R.renderHtml $ do
    mapM_ renderYearGroup sortedGroupedEntries
  where
    -- sort newest-first, then group by year
    sortedEntries        = sortOn (Down . entryYear) entries
    groupedEntries       = groupBy ((==) `on` entryYear) sortedEntries
    sortedGroupedEntries = map (\es -> (entryYear (head es), es)) groupedEntries

    -- one block per year
    renderYearGroup (yr, es) = H.div H.! A.class_ "year-container" $ do
        H.h2  H.! A.class_ "year-label" $ H.toHtml (show yr)
        H.ul  H.! A.class_ "publist"    $ mapM_ (entryToHTML showBib) es

-- Function to write HTML to a file
writeHtmlToFile :: FilePath -> String -> IO ()
writeHtmlToFile filePath htmlContent = writeFile filePath htmlContent



