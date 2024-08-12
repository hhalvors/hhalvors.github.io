--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid                   (mappend)
import           Data.List                     (sortBy)
import           Data.Ord                      (comparing)
import           Hakyll
import           Control.Monad                 (liftM, forM_)
import           System.FilePath               (takeBaseName, takeFileName)
import           BibTeXParser                  (parseBibTeX, generateHTML, transformEntry) 

--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration
  { destinationDirectory = "docs"
  }

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

    match "hh.bib" $ do
      compile getResourceBody

    -- Rule to process temp.html and output it as publications.html
    match "temp.html" $ do
      route $ customRoute (const "publications.html")
      compile $ do
        let titleCtx = constField "title" "Publications" `mappend` siteCtx
        getResourceBody
          >>= loadAndApplyTemplate "templates/page.html" titleCtx
          >>= loadAndApplyTemplate "templates/default.html" (titleCtx `mappend` baseSidebarCtx)
          >>= relativizeUrls
  
    -- Rule to process courses-temp.html and output it as courses.html
    match "courses-temp.html" $ do
      route $ customRoute (const "courses.html")
      compile $ do
        getResourceBody
          >>= loadAndApplyTemplate "templates/page.html" (constField "title" "Courses" `mappend` siteCtx)
          >>= loadAndApplyTemplate "templates/default.html" (baseSidebarCtx <> siteCtx)
          >>= relativizeUrls

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

    -- Bohr: Process all .md files in the "bohr" folder
    match "bohr/*.md" $ do
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

    -- .nojekyll
    create [".nojekyll"] $ do
        route idRoute
        compile copyFileCompiler


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

-----

myPandocBiblioCompiler :: Compiler (Item String)
myPandocBiblioCompiler =
  pandocBiblioCompiler "bib/style.csl" "bib/bibliography.bib"

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
    , feedRoot        = "https://hanshalvorson.dk"
    }

--------------------------------------------------------------------------------

siteCtx :: Context String
siteCtx =
    baseCtx `mappend`
    constField "site_description" "Personligheden er Sandheden" `mappend`
    constField "site-url" "https://hanshalvorson.dk" `mappend`
    constField "tagline" "Physics, Logic, Philosophy" `mappend`
    constField "site-title" "Hans Halvorson" `mappend`
    constField "copy-year" "2024" `mappend`
    constField "github-repo" "https://github.com/hhalvors" `mappend`
    defaultContext

--- change baseurl to test on local machine    

baseCtx = 
--  constField "baseurl" "http://localhost:8000"
    constField "baseurl" "https://hanshalvorson.dk"

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
