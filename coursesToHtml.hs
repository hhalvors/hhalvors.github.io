{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text, unpack, pack)
import qualified Data.Text as T
import Text.Blaze.Html5 as H hiding (map, main)
import Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.String as R
import Data.Char (toLower)
import Data.List (sortBy)
import Data.Ord (comparing)
import Courses (Course(..), courses) -- Import Course and courses from the Courses module

termToShort :: Text -> Text
termToShort term
    | "fall" `elem` termWords = "f" <> year
    | "spring" `elem` termWords = "s" <> year
    | "summer" `elem` termWords = "su" <> year
    | otherwise = year
  where
    termWords = words $ map toLower $ unpack term
    year = pack $ last termWords

-- Extract year and season from courseTerm
parseTerm :: Text -> (Int, String)
parseTerm term =
    let termStr = unpack term
        (season, year) = break (== ' ') termStr
    in (read (drop 1 year) :: Int, season)

-- Custom comparison function
compareCourseTerm :: Course -> Course -> Ordering
compareCourseTerm c1 c2 =
    let (year1, season1) = parseTerm (T.pack (courseTerm c1))
        (year2, season2) = parseTerm (T.pack (courseTerm c2))
    in case compare year2 year1 of
         EQ -> compareSeason season1 season2
         other -> other

-- Comparison function for seasons
compareSeason :: String -> String -> Ordering
compareSeason s1 s2 = compare s1 s2

-- Main function to sort courses and generate HTML
main :: IO ()
main = do
    let sortedCourses = sortBy compareCourseTerm courses
    let htmlContent = R.renderHtml $ generateHtml sortedCourses
    writeFile "courses.html" htmlContent
    putStrLn "HTML file 'courses.html' generated successfully."

generateHtml :: [Course] -> Html
generateHtml courses = docTypeHtml $ do
    H.head $ do
        H.title "Courses Taught"
        H.style $ toHtml $ unlines
            [ "body { font-family: Arial, sans-serif; margin: 40px; }"
            , "h1 { color: #333; }"
            , "table { width: 50%; border-collapse: collapse; }"
            , "th, td { border: 1px solid #ccc; padding: 10px; }"
            , "th { background-color: #f4f4f4; }"
            , "tr:nth-child(even) { background-color: #f9f9f9; }"
            , "a { color: #3498db; text-decoration: none; }"
            , "a:hover { text-decoration: underline; }"
            , "th.column1, td.column1 { width: 100px; text-align: left; }"
            , "th.column2, td.column2 { width: 100px; text-align: center; }"
            ]
    H.body $ do
        H.h1 "Courses I have taught"
        H.table $ do
            H.tr $ do
                H.th "Name"
                H.th ! A.class_ "column2" $ "Number"
                H.th "Term"
            mapM_ renderCourse courses

renderCourse :: Course -> Html
renderCourse course = H.tr $ do
    H.td $ toHtml $ courseName course
    H.td ! A.class_ "column2" $ 
        case courseUrl course of
            url | not (null url) -> H.a ! A.href (toValue url) $ toHtml $ courseNumber course
            _ -> toHtml $ courseNumber course
    H.td $ toHtml $ courseTerm course

