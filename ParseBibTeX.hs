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

-- Function to generate HTML content with articles sorted by year
generateHtml :: [T] -> String
generateHtml entries = R.renderHtml $ H.docTypeHtml $ do
    H.head $ H.title "BibTeX Entries"
    H.body $ do
        H.h1 "List of Articles"
        mapM_ renderYearGroup sortedGroupedEntries
  where
    sortedEntries = sortOn (Down . entryYear) entries
    groupedEntries = groupBy ((==) `on` entryYear) sortedEntries
    sortedGroupedEntries = map (\es -> (entryYear (head es), es)) groupedEntries

    renderYearGroup (year, entries) = do
        H.h2 (H.toHtml $ show year)
        H.ul $ mapM_ (H.li . renderEntry) entries

-- Helper function to extract the title from a BibTeX entry
entryTitle :: T -> String
entryTitle (Cons _ _ fields) = maybe "No Title" id (lookup "title" fields)

-- Helper function to extract the year from a BibTeX entry
entryYear :: T -> Int
entryYear (Cons _ _ fields) = maybe 0 read (lookup "year" fields)

-- Helper function to extract the DOI from a BibTeX entry
entryDOI :: T -> Maybe String
entryDOI (Cons _ _ fields) = lookup "doi" fields

-- Function to render an entry with a possible DOI hyperlink
renderEntry :: T -> H.Html
renderEntry entry = do
    let title = entryTitle entry
    let maybeDOI = entryDOI entry
    H.toHtml title
    case maybeDOI of
      Just doi -> H.span $ H.a H.! A.href (H.toValue $ "https://doi.org/" ++ doi) $ H.toHtml (" doi:" ++ doi)
      Nothing -> return ()

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

