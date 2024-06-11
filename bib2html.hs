{-# LANGUAGE OverloadedStrings #-}

import Text.CSL
import Text.CSL.Pandoc
import Text.Pandoc
import Text.Pandoc.Citeproc (processCitations)
import System.Environment (getArgs)

-- Function to process the BibTeX file and generate HTML
processBibFile :: FilePath -> FilePath -> FilePath -> IO ()
processBibFile bibFile cslFile outputFile = do
    let readerOptions = def { readerExtensions = pandocExtensions }
    let csl = readCSLFile Nothing cslFile
    bibContent <- readFile bibFile
    let bib = readBiblioFile BibTeX bibFile bibContent
    let doc = processCitations readerOptions def (processCites csl) mempty bib
    writeFile outputFile (writeHtmlString def doc)

-- Main function
main :: IO ()
main = do
    args <- getArgs
    case args of
        [bibFile, cslFile, outputFile] -> do
            processBibFile bibFile cslFile outputFile
        _ -> putStrLn "Usage: stack runhaskell <bibFile> <cslFile> <outputFile>"




