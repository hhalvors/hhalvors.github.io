{-# LANGUAGE OverloadedStrings #-}

import BibTeXParser (parseBibTeX, generateHTML, transformEntry)
import System.IO (writeFile)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [inputFilePath, outputFilePath] -> do
            bibContent <- readFile inputFilePath
            let bibEntries = either (error . show) id $ parseBibTeX bibContent
            let htmlContent = generateHTML (map transformEntry bibEntries)
            writeFile outputFilePath htmlContent
            putStrLn $ "HTML output written to " ++ outputFilePath
        _ -> putStrLn "Usage: runghc GenerateBibHtml.hs <path-to-bibtex-file> <output-html-file>"
