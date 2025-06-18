{-# LANGUAGE OverloadedStrings #-}

import Text.BibTeX.Parse (file)
import Text.BibTeX.Entry (T(..))
import Text.Parsec (ParseError, eof, parse)
import Text.Parsec.String (Parser)
import System.IO (hPutStrLn, stderr)
import System.Environment (getArgs)
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

generateHtml :: [T] -> String
generateHtml entries = R.renderHtml $ do
    H.h1 "Publications"
    mapM_ renderYearGroup sortedGroupedEntries
  where
    sortedEntries        = sortOn (Down . entryYear) entries
    groupedEntries       = groupBy ((==) `on` entryYear) sortedEntries
    sortedGroupedEntries = map (\es -> (entryYear (head es), es)) groupedEntries

    renderYearGroup (year, es) = do
      H.div H.! A.class_ "year-container" $ do
        H.h2 H.! A.class_ "year-label" $ H.toHtml (show year)
        H.ul H.! A.class_ "publist"     $ mapM_ renderEntry es    

-- Function to convert a BibTeX entry to an HTML string using blaze-html, omitting certain keys
entryToHTML :: T -> H.Html
entryToHTML (Cons entryType entryKey fields) = do
  H.span H.! A.class_ "buttonline" $ do
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
        

-- Function to render the HTML to a string
renderEntryToHTMLString :: T -> String
renderEntryToHTMLString = R.renderHtml . entryToHTML

-- Helper function to extract the DOI from a BibTeX entry
entryDOI :: T -> Maybe String
entryDOI (Cons _ _ fields) = lookup "doi" fields

-- Helper function to extract the year from a BibTeX entry
entryYear :: T -> Int
entryYear (Cons _ _ fields) = maybe 0 read (lookup "year" fields)

-- Helper function to extract the title from a BibTeX entry
entryTitle :: T -> String
entryTitle (Cons _ _ fields) = fromMaybe "No Title" (lookup "title" fields)

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
                           H.br
                           entryToHTML entry
      (Nothing, Just url) -> H.span $ do
                           H.a H.! A.href (H.toValue url) $ H.toHtml title
                           renderJournal
                           H.br
                           entryToHTML entry
      (Nothing, Nothing) -> H.span $ do
                           H.toHtml title
                           renderJournal
                           H.br
                           entryToHTML entry

-- Function to extract the URL from a BibTeX entry
entryURL :: T -> Maybe String
entryURL (Cons _ _ fields) = lookup "url" fields

-- Function to extract the journal from a BibTeX entry
entryJournal :: T -> Maybe String
entryJournal (Cons _ _ fields) = lookup "journal" fields

-- Function to write HTML to a file
writeHtmlToFile :: FilePath -> String -> IO ()
writeHtmlToFile filePath htmlContent = writeFile filePath htmlContent

-- Main function to read the file path from command line arguments, parse it, and generate HTML
main :: IO ()
main = do
    args <- getArgs
    case args of
        [inputFilePath, outputFilePath] -> do
            result <- parseBibTeXFile inputFilePath
            case result of
                Left err -> hPutStrLn stderr ("Error: " ++ show err)
                Right entries -> do
                    let transformedEntries = map transformEntry entries
                    let htmlContent = generateHtml transformedEntries
                    writeHtmlToFile outputFilePath htmlContent
                    putStrLn ("HTML output written to " ++ outputFilePath)
        _ -> hPutStrLn stderr "Usage: runghc ParseBibTeX.hs <path-to-bibtex-file> <path-to-output-html-file>"

