{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module Main where

-- Core / utils
import           Control.Applicative      ((<|>))
import           Control.Monad            (forM_, when, liftM, (<=<))
import           Data.Aeson               (FromJSON(..), withObject, (.:), (.:?), eitherDecode)
import qualified Data.ByteString.Lazy     as BL
import           Data.Function            (on)
import           Data.List                (groupBy, sortOn, sortBy)
import           Data.Maybe               (fromMaybe)
import           Data.Monoid              (mappend)
import           Data.Ord                 (Down(..), comparing)
import System.FilePath (splitDirectories, joinPath, takeExtension, takeBaseName, takeFileName)
import Text.Regex.TDFA ((=~))
import Text.Read (readMaybe)
import Data.Yaml (decodeFileEither)

-- Text / process
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import           GHC.IO.Handle            (hSetBuffering, BufferMode(NoBuffering))
import           System.Process           (runInteractiveCommand)

-- Hakyll / Pandoc
import           Hakyll
import qualified Text.Pandoc.Options      as P
import           Text.Pandoc              (pandocExtensions)
import           Text.Pandoc.SideNote     (usingSideNotes)
import           Text.Pandoc.Definition   (Pandoc, Inline(..), MathType(..))
import           Text.Pandoc.Walk         (walkM)

import           BibTeXParser             (parseBibTeX, generateHTML)
import           PubList                  (parseBibTeXFile, transformEntry, generateHtml)
import           LemmonFilter             (applyLemmonFilter)
import CoursePages.Course
  ( loadCourseYaml
  , compileLecturesFromCourseYaml
  , compilePsetsFromCourseYaml
  , courseBaseCtx
  , courseSiteCtx
  , courseNavItemCtx
  , compilePreceptsFromCourseYaml
  )

config :: Configuration
config = defaultConfiguration
  { destinationDirectory = "docs"
  }

myReader :: P.ReaderOptions
myReader =
  let exts = foldr P.enableExtension (P.readerExtensions defaultHakyllReaderOptions)
               [ P.Ext_tex_math_dollars
               , P.Ext_tex_math_double_backslash
               , P.Ext_latex_macros
               , P.Ext_raw_tex
               , P.Ext_raw_html
               ]
  in defaultHakyllReaderOptions { P.readerExtensions = exts }

myWriter :: P.WriterOptions
myWriter =
  let exts = foldr P.enableExtension (P.writerExtensions defaultHakyllWriterOptions)
               [ P.Ext_tex_math_dollars
               , P.Ext_tex_math_double_backslash
               , P.Ext_latex_macros
               , P.Ext_raw_tex
               , P.Ext_raw_html
               ]
  in defaultHakyllWriterOptions { P.writerExtensions = exts
                                , P.writerHTMLMathMethod = P.PlainMath
                                  -- We will inject *rendered* HTML ourselves
                                }

-- Simple HTML for a meta-refresh redirect
redirectPage :: String -> String
redirectPage target = mconcat
  [ "<!DOCTYPE html><html lang='en'><head><meta charset='utf-8'>"
  , "<meta http-equiv='refresh' content='0; url=", target, "'>"
  , "<link rel='canonical' href='", target, "'>"
  , "<title>Redirecting…</title></head><body>"
  , "<p>If you are not redirected automatically, "
  , "<a href='", target, "'>click here</a>.</p>"
  , "</body></html>"
  ]

-- Helper to create one output file with that HTML
redirect :: Identifier -> String -> Rules ()
redirect ident target =
  create [ident] $ do
    route idRoute
    compile $ makeItem (redirectPage target)     

-- Server-side KaTeX: replace Math inlines/blocks with KaTeX HTML via Deno script
hlKaTeX :: Pandoc -> Compiler Pandoc
hlKaTeX doc = unsafeCompiler $ do
  -- Allow reading the local script and fetching katex.mjs from jsdelivr:
  let cmd = "deno run --allow-read --allow-net=cdn.jsdelivr.net scripts/math.ts"
  (hin, hout, _, _) <- runInteractiveCommand cmd
  hSetBuffering hin  NoBuffering
  hSetBuffering hout NoBuffering

  let renderOne :: Bool -> T.Text -> IO T.Text
      renderOne isDisplay t = do
        let line = if isDisplay then T.pack ":DISPLAY " <> t else t
        T.hPutStrLn hin (T.strip line)
        T.hGetLine hout  -- we made math.ts print a single line per input

      go :: Inline -> IO Inline
      go (Math InlineMath  t) = RawInline "html" <$> renderOne False t
      go (Math DisplayMath t) = RawInline "html" <$> renderOne True  t
      go x                    = pure x

  walkM go doc  


------------------------------------------------------------------------------
-- Course data  +  FromJSON instance
------------------------------------------------------------------------------
data Course = Course
  { title    :: String
  , code     :: String
  , semester :: String
  , year     :: Int
  , link     :: Maybe String
  } deriving Show

instance FromJSON Course where
  parseJSON = withObject "Course" $ \o ->
      Course <$> o .:  "title"
             <*> o .:  "code"
             <*> o .:  "semester"
             <*> o .:  "year"
             <*> o .:? "link"

------------------------------------------------------------------------------
-- Load and decode JSON once at compile time
------------------------------------------------------------------------------
loadAllCourses :: Compiler [Course]
loadAllCourses = unsafeCompiler $ do
  bs <- BL.readFile "data/courses.json"
  either (fail . ("JSON decode error: " <>)) pure (eitherDecode bs)

------------------------------------------------------------------------------
-- Context for a single course
------------------------------------------------------------------------------
courseCtx :: Context Course
courseCtx =
     field "title"    (return . title    . itemBody)
  <> field "code"     (return . code     . itemBody)
  <> field "semester" (return . semester . itemBody)
  <> field "year"     (return . show . year . itemBody)
  <> field "link"     (return . maybe "#" id . link . itemBody)
  <> field "hasLink"  (return . maybe "false" (const "true") . link . itemBody)
  
------------------------------------------------------------------------------
-- Rule: build /courses/index.html
------------------------------------------------------------------------------

yearCtx :: Context ([Course], Int)   -- ([coursesThisYear], year)
yearCtx =
       field "year"     (return . show . snd . itemBody)
    <> listFieldWith "courses" courseCtx (\item -> mapM makeItem (fst $ itemBody item))

isPlainPsetPdf :: Identifier -> Bool
isPlainPsetPdf ident =
  takeFileName (toFilePath ident) =~ ("^pset[0-9]+\\.pdf$" :: String)

psetNumber :: Identifier -> Int
psetNumber ident =
  let fname = takeFileName (toFilePath ident)
      ds    = takeWhile (`elem` ['0'..'9']) (drop 4 fname)
  in maybe 0 id (readMaybe ds)       

psetCtx :: Context CopyFile
psetCtx =
  (field "number" $ \item -> do
     let fname = takeBaseName . toFilePath $ itemIdentifier item  -- "pset7"
     pure (drop 4 fname)                                         -- "7"
  )
  <>
  (field "url" $ \item ->
     pure . toUrl . toFilePath $ itemIdentifier item
  )       

buildCoursesPage :: Rules ()
buildCoursesPage =
  create ["courses/index.html"] $ do
    route idRoute
    compile $ do
      -- load & sort newest-first
      cs <- loadAllCourses
      let sorted = sortOn (Down . year) cs

          -- group into [(year,[Course])]
          groups :: [(Int,[Course])]
          groups = map (\ys -> (year $ head ys, ys))
                 . groupBy ((==) `on` year) $ sorted

      -- turn each year-group into an Item ([Course],Int)
      yearItems <- mapM (makeItem . (\(y,xs)->(xs,y))) groups

      let pageCtx =
            listField "years" yearCtx (return yearItems)
            <> baseSidebarCtx                 -- supplies list_pages etc.
            <> constField "title" "Courses Taught"
            <> siteCtx
      
      makeItem ("" :: String)
        >>= loadAndApplyTemplate "templates/courses.html"  pageCtx
        >>= loadAndApplyTemplate "templates/default.html"  pageCtx
        >>= relativizeUrls


main :: IO ()
main = hakyllWith config $ do
    match ("images/*" .||. "js/*") $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "bib/style.csl"        $ compile cslCompiler

    match "bib/bibliography.bib" $ compile biblioCompiler    

    match "CNAME" $ do 
        route   idRoute
        compile copyFileCompiler    

    match "error/*" $ do
        route $ (gsubRoute "error/" (const "") `composeRoutes` setExtension "html")
        compile $ pandocCompiler
            >>= applyAsTemplate siteCtx
            >>= loadAndApplyTemplate "templates/default.html" (baseSidebarCtx <> siteCtx)

    match "pages/index.md" $ do
      route $ constRoute "index.html"
      compile $ do
        let indexCtx = constField "title" "Home" `mappend` siteCtx
        pandocCompiler
          >>= saveSnapshot "page-content"
          >>= loadAndApplyTemplate "templates/page.html" siteCtx
          >>= loadAndApplyTemplate "templates/default.html" (baseSidebarCtx <> indexCtx)
          >>= relativizeUrls

    match "courses/*.html" $ do
      route   idRoute
      compile $ do
        let pageCtx = constField "title" "Course" <> siteCtx
        getResourceBody
          >>= readPandoc -- parses the HTML file into Pandoc AST
          >>= return . writePandoc
          >>= loadAndApplyTemplate "templates/page.html" pageCtx
          >>= loadAndApplyTemplate "templates/default.html" (baseSidebarCtx <> pageCtx)
          >>= relativizeUrls      

    -- match "courses/index.md" $ do
    --   route $ constRoute "courses/index.html"
    --   compile $ do
    --     let indexCtx = constField "title" "test" `mappend` siteCtx
    --     pandocCompiler
    --       >>= saveSnapshot "page-content"
    --       >>= loadAndApplyTemplate "templates/page.html" siteCtx
    --       >>= loadAndApplyTemplate "templates/default.html" (baseSidebarCtx <> indexCtx)
    --       >>= relativizeUrls

    match "courses/phi201_f2025/index.md" $ do
      route $ setExtension "html"
      compile $ do
        ctx <- courseBaseCtx "courses/phi201_f2025" "home"
        pandocCompiler
          >>= loadAndApplyTemplate "templates/course-home.html" ctx
          >>= loadAndApplyTemplate "templates/course-base.html" ctx
          >>= relativizeUrls

    match "courses/phi201_f2025/resources.md" $ do
      route $ constRoute "courses/phi201_f2025/resources.html"
      compile $ do
        ctx <- courseBaseCtx "courses/phi201_f2025" "resources"
        pandocCompiler
          >>= loadAndApplyTemplate "templates/course-base.html" ctx
          >>= relativizeUrls      

    match "courses/phi201_f2025/lectures.md" $ do
      route $ constRoute "courses/phi201_f2025/lectures.html"
      compile $ do
        ctx <- courseBaseCtx "courses/phi201_f2025" "lectures"
        compileLecturesFromCourseYaml
          "courses/phi201_f2025/course.yaml"
          "templates/lectures-grid.html"
          >>= loadAndApplyTemplate "templates/course-base.html" ctx
          >>= relativizeUrls

    match "courses/phi201_f2025/psets.md" $ do
      route $ constRoute "courses/phi201_f2025/psets.html"
      compile $ do
        ctx <- courseBaseCtx "courses/phi201_f2025" "psets"  
        compilePsetsFromCourseYaml
          "courses/phi201_f2025/course.yaml"
          "templates/psets-list.html"
          >>= loadAndApplyTemplate "templates/course-base.html" ctx
          >>= relativizeUrls

    match "courses/phi201_f2025/precepts.md" $ do
      route $ constRoute "courses/phi201_f2025/precepts.html"
      compile $ do
        ctx <- courseBaseCtx "courses/phi201_f2025" "precepts"
        compilePreceptsFromCourseYaml
          "courses/phi201_f2025/course.yaml"
          "templates/precepts-list.html"
          >>= loadAndApplyTemplate "templates/course-base.html" ctx
          >>= relativizeUrls

-- phi 220

    match "courses/phi221_f2026/index.md" $ do
      route $ setExtension "html"
      compile $ do
        ctx <- courseBaseCtx "courses/phi221_f2026" "home"
        pandocCompiler
          >>= loadAndApplyTemplate "templates/course-home.html" ctx
          >>= loadAndApplyTemplate "templates/course-base.html" ctx
          >>= relativizeUrls

    -- ============================================================
-- PHI 338 (Fall 2014) — legacy course: syllabus + lecture handouts
-- ============================================================

    match "courses/phi338_f2014/index.md" $ do
      route $ setExtension "html"
      compile $ do
        ctx <- courseBaseCtx "courses/phi338_f2014" "home"
        pandocCompiler
          >>= loadAndApplyTemplate "templates/course-home.html" ctx
          >>= loadAndApplyTemplate "templates/course-base.html" ctx
          >>= relativizeUrls

    match "courses/phi338_f2014/lectures.md" $ do
      route $ setExtension "html"
      compile $ do
        ctx <- courseBaseCtx "courses/phi338_f2014" "lectures"
        compileLecturesFromCourseYaml
          "courses/phi338_f2014/course.yaml"
          "templates/lectures-grid.html"
          >>= loadAndApplyTemplate "templates/course-base.html" ctx
          >>= relativizeUrls

-- ============================================================
-- PHI 327 (Spring 2020)
-- ============================================================

    match "courses/phi327_s2020/index.md" $ do
      route $ setExtension "html"
      compile $ do
        ctx <- courseBaseCtx "courses/phi327_s2020" "home"
        pandocCompiler
          >>= loadAndApplyTemplate "templates/course-home.html" ctx
          >>= loadAndApplyTemplate "templates/course-base.html" ctx
          >>= relativizeUrls

    match "courses/phi327_s2020/lectures.md" $ do
      route $ constRoute "courses/phi327_s2020/lectures.html"
      compile $ do
        ctx <- courseBaseCtx "courses/phi327_s2020" "lectures"
        compileLecturesFromCourseYaml
          "courses/phi327_s2020/course.yaml"
          "templates/lectures-grid.html"
          >>= loadAndApplyTemplate "templates/course-base.html" ctx
          >>= relativizeUrls

    match "courses/phi327_s2020/assignments.md" $ do
      route $ constRoute "courses/phi327_s2020/assignments.html"
      compile $ do
        ctx <- courseBaseCtx "courses/phi327_s2020" "assignments"
        pandocCompiler
          >>= loadAndApplyTemplate "templates/course-base.html" ctx
          >>= relativizeUrls

    match "courses/phi327_s2020/resources.md" $ do
      route $ constRoute "courses/phi327_s2020/resources.html"
      compile $ do
        ctx <- courseBaseCtx "courses/phi327_s2020" "resources"
        pandocCompiler
          >>= loadAndApplyTemplate "templates/course-base.html" ctx
          >>= relativizeUrls

---- end of course block
      

    match ("courses/**.md" .&&. complement "courses/phi201_f2025/index.md") $ do
      route $ setExtension "html"
      compile $
        myPandocBiblioCompiler
          >>= loadAndApplyTemplate "templates/page.html" (defaultContext <> siteCtx)
          >>= loadAndApplyTemplate "templates/default.html" (baseSidebarCtx <> siteCtx)
          >>= relativizeUrls      

    match "pages/john.md" $ do
      route $ constRoute "john.html"
      compile $ do
        let indexCtx = constField "title" "Home" `mappend` siteCtx
        pandocCompiler
          >>= saveSnapshot "page-content"
          >>= loadAndApplyTemplate "templates/page.html" siteCtx
          >>= loadAndApplyTemplate "templates/default.html" (baseSidebarCtx <> indexCtx)
          >>= relativizeUrls

    match "books/*.md" $ do
      route $ setExtension "html"
      compile $
        pandocCompiler
          >>= saveSnapshot "page-content"
          >>= loadAndApplyTemplate "templates/page.html" siteCtx
          >>= loadAndApplyTemplate "templates/default.html" (baseSidebarCtx <> siteCtx)
          >>= relativizeUrls

-- books/*.pdf  -> copied verbatim (e.g., books/hlw-solutions.pdf)
    match "books/*.pdf" $ do
      route   idRoute
      compile copyFileCompiler      

    redirect "phi201/index.html" "https://hanshalvorson.com/courses/phi201_f2025/"

    match "pages/*" $ do
      route $ setExtension "html"
      compile $ do
        pageName <- takeBaseName . toFilePath <$> getUnderlying
        let pageCtx = constField pageName "" `mappend` baseNodeCtx
        let evalCtx = functionField "get-meta" getMetadataKey `mappend`
                      functionField "eval" (evalCtxKey pageCtx)
        let activeSidebarCtx = sidebarCtx (evalCtx <> pageCtx)
        
        pandocCompiler
          >>= saveSnapshot "page-content"
          >>= loadAndApplyTemplate "templates/page.html" siteCtx
          >>= loadAndApplyTemplate "templates/default.html" (activeSidebarCtx <> siteCtx)
          >>= relativizeUrls
  
    match "pandoc/*.bib" $ 
        compile biblioCompiler

    match "pandoc/elsevier.csl" $
        compile cslCompiler

    match "debug.md" $ do
      route $ setExtension "html"
      compile $
        pandocCompilerWith myReader myWriter
          >>= relativizeUrls    

    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    -- match "posts/*" $ version "meta" $ do
    --     route   $ setExtension "html"
    --     compile getResourceBody

    -- match "posts/*" $ do
    --     route $ setExtension "html"
    --     compile $ do
    --         posts <- loadAll ("posts/*" .&&. hasVersion "meta")
    --         let taggedPostCtx = (tagsField "tags" tags) `mappend`
    --                             postCtx `mappend`
    --                             (relatedPostsCtx posts 3)

    --         pandocCompiler
    --             >>= saveSnapshot "content"
    --             >>= loadAndApplyTemplate "templates/post.html" taggedPostCtx
    --             >>= loadAndApplyTemplate "templates/default.html" (baseSidebarCtx <> siteCtx)
    --             >>= relativizeUrls

    -- create ["archive.html"] $ do
    --     route idRoute
    --     compile $ do
    --         posts <- recentFirst =<< loadAllSnapshots ("posts/*" .&&. hasNoVersion) "content"
    --         let archiveCtx =
    --                 listField "posts" postCtx (return posts) `mappend`
    --                 constField "title" "Archive"             `mappend`
    --                 constField "archive" ""                  `mappend`
    --                 siteCtx

    --         makeItem ""
    --             >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
    --             >>= loadAndApplyTemplate "templates/default.html" (baseSidebarCtx <> archiveCtx)
    --             >>= relativizeUrls

    -- paginate <- buildPaginateWith postsGrouper "posts/*" postsPageId

    -- paginateRules paginate $ \page pattern -> do
    --     route idRoute
    --     compile $ do
    --         posts <- recentFirst =<< loadAllSnapshots (pattern .&&. hasNoVersion) "content"
    --         let indexCtx =
    --                 constField "title" (if page == 1 then "Home"
    --                                                  else "Blog posts, page " ++ show page) `mappend`
    --                 listField "posts" postCtx (return posts) `mappend`
    --                 constField "home" "" `mappend`
    --                 paginateContext paginate page `mappend`
    --                 siteCtx

    --         makeItem ""
    --             >>= applyAsTemplate indexCtx
    --             >>= loadAndApplyTemplate "templates/index.html" indexCtx
    --             >>= loadAndApplyTemplate "templates/default.html" (baseSidebarCtx <> indexCtx)
    --             >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

    -- Copy any PDFs under courses/ (recursively)
    match "courses/**.pdf" $ do
        route   idRoute
        compile copyFileCompiler

    -- Copy any TeX files under courses/ (recursively)
    match "courses/**.tex" $ do
        route   idRoute
        compile copyFileCompiler

    create ["logic-exams-page"] $ do
      route $ constRoute "courses/logic-exams/index.html"
      compile $ do
        exams <- (loadAll "courses/logic-exams/*.pdf" :: Compiler [Item CopyFile])
        unsafeCompiler $ mapM_ (putStrLn . ("DBG url: " ++) . toUrl . toFilePath . itemIdentifier) (take 3 exams)

        let examCtx :: Context CopyFile
            examCtx =
                 field "url"  (pure . toFilePath . itemIdentifier)
              <> field "name" (pure . takeBaseName . toFilePath . itemIdentifier)

            pageCtx :: Context String
            pageCtx =
                 constField "title" "intro logic exams"
              <> listField "exams" examCtx (pure exams)
              <> baseSidebarCtx
              <> siteCtx

        makeItem ("" :: String)
          >>= loadAndApplyTemplate "templates/logic-exams-list.html" pageCtx
          >>= loadAndApplyTemplate "templates/page.html"            pageCtx
          >>= loadAndApplyTemplate "templates/default.html"         pageCtx

    match "hh.bib" $ do
      compile getResourceBody

    create ["publications.html"] $ do
      route idRoute
      compile $ do
        result <- unsafeCompiler $ parseBibTeXFile "hh.bib"
        case result of
          Left err -> error $ "BibTeX parse error: " ++ show err
          Right entries -> do
            let htmlBody = generateHtml False (map PubList.transformEntry entries)
            makeItem htmlBody
              >>= loadAndApplyTemplate "templates/page.html"    (constField "title" "Publications" `mappend` siteCtx)
              >>= loadAndApplyTemplate "templates/default.html" (constField "title" "Publications" `mappend` baseSidebarCtx `mappend` siteCtx)
              >>= relativizeUrls

-- Rule to process temp.html and output it as publications.html
    -- match "temp.html" $ do
    --   route $ customRoute (const "publications.html")
    --   compile $ do
    --     let titleCtx = constField "title" "Publications" `mappend` siteCtx
    --     getResourceBody
    --       >>= loadAndApplyTemplate "templates/page.html" titleCtx
    --       >>= loadAndApplyTemplate "templates/default.html" (titleCtx `mappend` baseSidebarCtx)
    --       >>= relativizeUrls
  
    -- Rule to process courses-temp.html and output it as courses.html
--    match "courses-temp.html" $ do
--      route $ customRoute (const "courses.html")
--      compile $ do
--        getResourceBody
--          >>= loadAndApplyTemplate "templates/page.html" (constField "title" "Courses" `mappend` siteCtx)
--          >>= loadAndApplyTemplate "templates/default.html" (baseSidebarCtx <> siteCtx)
--          >>= relativizeUrls

    -- -- Rule to process courses-temp.html and output it as courses/index.html
    -- match "courses-temp.html" $ do
    --   route $ customRoute (const "courses/index.html")
    --   compile $ do
    --     getResourceBody
    --       >>= loadAndApplyTemplate "templates/page.html" (constField "title" "Courses" `mappend` siteCtx)
    --       >>= loadAndApplyTemplate "templates/default.html" (baseSidebarCtx <> siteCtx)
    --       >>= relativizeUrls
       

    -- Rule to process bohr-causality-quotes.html without applying templates or relativizing URLs
    match "bohr/causality-quotes.html" $ do
      route idRoute
      compile getResourceBody      

    -- Talks
    match "talks/**" $ do
        route idRoute
        compile copyFileCompiler      

    -- Talks
    match "talks.md" $ do
        route $ customRoute (const "talks.html")
        compile $ do
            pandocCompiler
                >>= loadAndApplyTemplate "templates/page.html" (constField "title" "Talks" `mappend` siteCtx)
                >>= loadAndApplyTemplate "templates/default.html" (baseSidebarCtx <> siteCtx)
                >>= relativizeUrls

    -- -- Bohr
    -- match "bohr/index.md" $ do
    --     route $ setExtension "html"
    --     compile $ do
    --         myPandocBiblioCompiler
    --             >>= loadAndApplyTemplate "templates/page.html" (constField "title" "Niels Bohr: Philosopher in Action" `mappend` siteCtx)
    --             >>= loadAndApplyTemplate "templates/default.html" (baseSidebarCtx <> siteCtx)
    --             >>= relativizeUrls

    -- Bohr: Process all .md files in the "kierkegaard" folder
    match "kierkegaard/*.md" $ do
      route $ setExtension "html"
      compile $ do
        myPandocBiblioCompiler
            >>= loadAndApplyTemplate "templates/page.html" (defaultContext `mappend` siteCtx)
            >>= loadAndApplyTemplate "templates/default.html" (baseSidebarCtx <> siteCtx)
    

    -- Bohr: Process all .md files in the "bohr" folder
    match "bohr/*.md" $ do
      route $ setExtension "html"
      compile $ do
        myPandocBiblioCompiler
            >>= loadAndApplyTemplate "templates/page.html" (defaultContext `mappend` siteCtx)
            >>= loadAndApplyTemplate "templates/default.html" (baseSidebarCtx <> siteCtx)

    match "bohr/bohr1960unity.tex" $ do
      route $ setExtension "html"
      compile $
        pandocCompilerWithTransformM
          defaultHakyllReaderOptions
          myWriter
          (pure . usingSideNotes)   -- Apply the sidenote transformation here
        >>= loadAndApplyTemplate "templates/page.html"
            ( mconcat
              [ constField "title" "The Unity of Human Knowledge"
              , constField "author" "Niels Bohr (with commentary by Hans Halvorson)"
              , defaultContext
              , siteCtx
              ]
            )
        >>= loadAndApplyTemplate "templates/default.html"
        ( baseSidebarCtx <> siteCtx <> constField "extraStylesheet" "../css/sidenotes.css" )    
        >>= relativizeUrls 

    match "spacetime/*.md" $ do
      route $ setExtension "html"
      compile $ do
        myPandocBiblioCompiler
            >>= loadAndApplyTemplate "templates/page.html" (defaultContext `mappend` siteCtx)
            >>= loadAndApplyTemplate "templates/default.html" (baseSidebarCtx <> siteCtx)            

        -- Logic: Process all .md files in the "logic" folder
    match "logic/*.md" $ do
      route $ setExtension "html"
      compile $ do
        myPandocBiblioCompiler
            >>= loadAndApplyTemplate "templates/page.html" (defaultContext `mappend` siteCtx)
            >>= loadAndApplyTemplate "templates/default.html" (baseSidebarCtx <> siteCtx)        

    -- Drafts
    match "drafts.md" $ do
        route $ customRoute (const "drafts.html")
        compile $ do
            pandocCompiler
                >>= loadAndApplyTemplate "templates/page.html" (constField "title" "Drafts" `mappend` siteCtx)
                >>= loadAndApplyTemplate "templates/default.html" (baseSidebarCtx <> siteCtx)
                >>= relativizeUrls                

    -- Ad Hoc: phi201_s2021
    match "phi201_s2021.md" $ do
        route $ customRoute (const "phi201_s2021.html")
        compile $ do
            pandocCompiler
                >>= loadAndApplyTemplate "templates/page.html" (constField "title" "Introductory Logic" `mappend` siteCtx)
                >>= loadAndApplyTemplate "templates/default.html" (baseSidebarCtx <> siteCtx)
                >>= relativizeUrls

    -- Rule to copy PDF files
    match "phi201_s2021_link/*.pdf" $ do
        route $ gsubRoute "phi201_s2021_link/" (const "phi201_s2021/")
        compile copyFileCompiler


    -- Rule to copy PDF files from the "pdf" directory
    match "pdf/*.pdf" $ do
        route idRoute
        compile copyFileCompiler

    match "courses/vtfys_2025/week4*.pdf" $ do
        route idRoute
        compile copyFileCompiler
    
    -- .nojekyll
    create [".nojekyll"] $ do
        route idRoute
        compile copyFileCompiler

    let currentPhi201 = "phi201_f2025"    -- <<< update this once per year
        courseRoot    = "courses/" ++ currentPhi201 ++ "/"

    redirect (fromFilePath "phi201/index.html") ("/" ++ courseRoot)

    -- Mirror every HTML page under the current course:
    --   /courses/phi201_f2025/pset1.html
    -- becomes a stub at
    --   /phi201/pset1.html  (meta-refresh to the target above)
    match (fromGlob (courseRoot ++ "**/*.html")) $ version "redirects" $ do
      route $ customRoute $ \i ->
        let p        = toFilePath i                          -- "courses/phi201_f2025/.../foo.html"
            comps    = splitDirectories p                    -- ["courses","phi201_f2025",...,"foo.html"]
            stripped = drop 2 comps                          -- [...,"foo.html"]
        in  joinPath ("phi201" : stripped)      
      compile $ do
        src <- toFilePath <$> getUnderlying                  -- original path under /courses/...
        makeItem (redirectPage ("/" ++ src))    

-- Rule to generate publications.html
--    create ["publications.html"] $ do 
--      route idRoute
--      compile $ do
--        bibContent <- loadBody "hh.bib"
--        let bibEntries = either (error . show) id $ parseBibTeX bibContent
--        let htmlContent = generateHTML (map transformEntry bibEntries)
--        makeItem htmlContent
--          >>= loadAndApplyTemplate "templates/page.html" siteCtx
--          >>= loadAndApplyTemplate "templates/default.html" (baseSidebarCtx <> siteCtx)
--          >>= relativizeUrls

    -- Rule to copy publications.html to /docs folder
    --    match "publications-new.html" $ do
    --  route $ customRoute (\_ -> "publications.html")
    --  compile copyFileCompiler          

    create ["atom.xml"] $ do
      route idRoute
      compile $ do
        let feedCtx = postCtx `mappend` bodyField "description"
        posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots ("posts/*" .&&. hasNoVersion) "content"
        renderAtom feedConfig feedCtx posts

    buildCoursesPage

-----

myPandocBiblioCompiler :: Compiler (Item String)
myPandocBiblioCompiler = do
  csl <- load "bib/style.csl"
  bib <- load "bib/bibliography.bib" :: Compiler (Item Biblio)
  pandocCompilerWithTransformM
    myReader
    myWriter
    (\p -> do
        processed <- processPandocBiblio csl bib (Item "" p)
        -- If you have a Lemmon filter, run it *before* KaTeX or ensure it skips Math
        let p1 = applyLemmonFilter (itemBody processed)
        hlKaTeX p1
    )

-- myPandocBiblioCompiler :: Compiler (Item String)
-- myPandocBiblioCompiler = do
--   csl <- load "bib/style.csl"
--   bib <- load "bib/bibliography.bib" :: Compiler (Item Biblio)

--   let mathExtensions = [ Ext_tex_math_dollars, Ext_tex_math_double_backslash, Ext_latex_macros, Ext_raw_tex, Ext_raw_html ]
--       newExtensions = foldr enableExtension (writerExtensions defaultHakyllWriterOptions) mathExtensions
--       writerOptions = defaultHakyllWriterOptions
--         { writerExtensions = newExtensions
--         , writerHTMLMathMethod = MathJax ""   
--         }

--   pandocCompilerWithTransformM
--     defaultHakyllReaderOptions
--     writerOptions
--     (\pandoc -> do
--         -- Apply bibliography processing
--         processed <- processPandocBiblio csl bib (Item "" pandoc)
--         -- Apply Lemmon filter to the processed result
--         return $ applyLemmonFilter (itemBody processed)
--     )



-- myPandocBiblioCompiler :: Compiler (Item String)
-- myPandocBiblioCompiler = do
--   let csl = "bib/style.csl"
--       bib = "bib/bibliography.bib"
  
--   pandocCompilerWithTransformM
--     defaultHakyllReaderOptions
--     defaultHakyllWriterOptions  -- Use default writer options
--     (return . applyLemmonFilter)  -- Apply the Lemmon filter

  
--- old one 
--- myPandocBiblioCompiler :: Compiler (Item String)
-- myPandocBiblioCompiler =
--   pandocBiblioCompiler "bib/style.csl" "bib/bibliography.bib"

--------------------------------------------------------------------------------

postsGrouper :: (MonadMetadata m, MonadFail m) => [Identifier] -> m [[Identifier]]
postsGrouper = liftM (paginateEvery 3) . sortRecentFirst

postsPageId :: PageNumber -> Identifier
postsPageId n = fromFilePath $ if (n == 1) then "index.html" else show n ++ "/index.html"

--------------------------------------------------------------------------------

feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
    { feedTitle       = "Physics, Logic, and Philosophy"
    , feedDescription = "Personligheden er Sandheden"
    , feedAuthorName  = "Hans Halvorson"
    , feedAuthorEmail = "hhalvors@princeton.edu"
    , feedRoot        = "https://hanshalvorson.com"
    }

--------------------------------------------------------------------------------

siteCtx :: Context String
siteCtx =
    baseCtx `mappend`
    constField "site_description" "Personligheden er Sandheden" `mappend`
    constField "site-url" "https://hanshalvorson.com" `mappend`
    constField "tagline" "Physics, Logic, Philosophy" `mappend`
    constField "site-title" "Hans Halvorson" `mappend`
    constField "copy-year" "2025" `mappend`
    constField "github-repo" "https://github.com/hhalvors" `mappend`
    defaultContext

--- change baseurl to test on local machine    

baseCtx = 
--  constField "baseurl" "http://localhost:8000"
    constField "baseurl" "https://hanshalvorson.com"

--------------------------------------------------------------------------------

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

tagsRulesVersioned tags rules =
    forM_ (tagsMap tags) $ \(tag, identifiers) ->
        rulesExtraDependencies [tagsDependency tags] $
            create [tagsMakeId tags tag] $
                rules tag identifiers

relatedPostsCtx
  :: [Item String]  -> Int  -> Context String
relatedPostsCtx posts n = listFieldWith "related_posts" postCtx selectPosts
  where
    rateItem ts i = length . filter (`elem` ts) <$> (getTags $ itemIdentifier i)
    selectPosts s = do
      postTags <- getTags $ itemIdentifier s
      let trimmedItems = filter (not . matchPath s) posts
      take n . reverse <$> sortOnM (rateItem postTags) trimmedItems

matchPath :: Item String -> Item String -> Bool
matchPath x y = eqOn (toFilePath . itemIdentifier) x y

eqOn :: Eq b => (a -> b) -> a -> a -> Bool
eqOn f x y = f x == f y

sortOnM :: (Monad m, Ord b) => (a -> m b) -> [a] -> m [a]
sortOnM f xs = map fst . sortBy (comparing snd) . zip xs <$> mapM f xs

--------------------------------------------------------------------------------

sidebarCtx :: Context String -> Context String
sidebarCtx nodeCtx =
    listField "list_pages" nodeCtx (loadAllSnapshots ("pages/*" .&&. hasNoVersion) "page-content") `mappend`
    defaultContext

baseNodeCtx :: Context String
baseNodeCtx =
    urlField "node-url" `mappend`
    titleField "title" `mappend`
    baseCtx

baseSidebarCtx = sidebarCtx baseNodeCtx

evalCtxKey :: Context String -> [String] -> Item String -> Compiler String
evalCtxKey context [key] item = (unContext context key [] item) >>= \cf ->
        case cf of
            StringField s -> return s
            _             -> error $ "Internal error: StringField expected"

getMetadataKey :: [String] -> Item String -> Compiler String
getMetadataKey [key] item = getMetadataField' (itemIdentifier item) key
