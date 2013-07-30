{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid (mappend, mconcat)
import Hakyll
import Data.Maybe (fromMaybe)
import Data.Map as M
import Text.Pandoc
import Text.Pandoc.Shared (stringify)

import Control.Monad (liftM)
import Data.Maybe (fromMaybe)
import Data.Monoid (mappend, mconcat)
import System.Locale (iso8601DateFormat)

import Text.Pandoc.Options
import Hakyll

main :: IO ()
main = hakyllWith config $ do

    let allPostsPattern = "posts/**/*.markdown"

    -- Build tags
    tags <- buildTags " " (fromCapture "tags/*.html")

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler    

    -- Compress CSS
    match "assets/css/*" $ do
        route   $ gsubRoute "assets/" (const "") `composeRoutes` idRoute
        compile compressCssCompiler

    -- Bibtex entries (for bibliography)
    match "assets/bib/*" $ compile biblioCompiler

    match "assets/csl/*" $ compile cslCompiler

    -- Copy static assets
    match "assets/files/*" $ do
        route   idRoute
        compile copyFileCompiler

    -- Render posts
    match allPostsPattern $ version "main" $ do
        route   $ setExtension ".html"
<<<<<<< HEAD
        compile $ pandocCompilerWithTransform defaultHakyllReaderOptions defaultHakyllWriterOptions {
                                         writerTemplate = "$body$"
                                        {-|
                                            unlines
                                            [ "$if(title)$"
                                            , "<h1 class=\"title\">$title$</h1>"
                                            , "$endif$"
                                            , "$for(include-before)$"
                                            , "$include-before$"
                                            , "$endfor$"
                                            , "$if(toc)$"
                                            , "$toc$"
                                            , "$endif$"
                                            , "$body$"
                                            , "$footnotes$"
                                            , "$for(include-after)$"
                                            , "$include-after$"
                                            , "$endfor$"
                                            ]
                                        -}
                                        , writerStandalone  = True
                                        , writerIgnoreNotes = True
                                        
                                    } removeNotes
    
            >>= loadAndApplyTemplate "templates/post_horizontal.html" (tagsCtx tags)
           -- >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls
=======
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" (tagsCtx tags)
            >>= saveSnapshot "content"
            >>= pageCompiler
>>>>>>> 77eb261fec70e8df3d52576c64865a4a8fc60730

            {-

    match allPostsPattern $ version "toc" $
       compile $ pandocCompilerWithTransform defaultHakyllReaderOptions
                                    defaultHakyllWriterOptions {
                                        writerTableOfContents = True
                                      , writerTemplate = "$body$"
                                      , writerStandalone = True
                                      } removeNotes    
            -}

    match allPostsPattern $ version "footnotes" $
       compile $ pandocCompilerWithTransform defaultHakyllReaderOptions
                                    defaultHakyllWriterOptions {
                                        writerTableOfContents = True
                                      , writerTemplate = "$body$"
                                      , writerStandalone = True
                                      } extractNotes                                  
            

    -- Render posts list
    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll (allPostsPattern .&&. hasNoVersion)
            itemTpl <- loadBody "templates/archive-item.html"
            list <- applyTemplateList itemTpl postCtx posts
            let archiveCtx = constField "title" "All posts" `mappend`
                             defaultContext
            makeItem list
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= pageCompiler

    -- Static pages
    match "pages/*" $ do
        route $ gsubRoute "pages/" (const "") `composeRoutes` setExtension "html"
        compile $ do
            item <- getUnderlying
            title <- liftM (fromMaybe "Related posts") $ getMetadataField item "relatedTitle"
            related <- liftM (fromMaybe "") $ getMetadataField item "related"
            bibFile <- liftM (fromMaybe "") $ getMetadataField item "biblio"
            cslFile <- liftM (fromMaybe "chicago") $ getMetadataField item "csl"
            list <- if related == "*" then
<<<<<<< HEAD
                        postList tags (allPostsPattern .&&. hasNoVersion) recentFirst
                    else let items = fromMaybe [] $ Prelude.lookup related (tagsMap tags)
                         in postList tags (Hakyll.fromList items) recentFirst

            let relatedCtx = constField "related.title" title `mappend`
                             constField "related" list `mappend`
                             defaultContext
            pandocCompiler
            --    >>= loadAndApplyTemplate "templates/related.html" relatedCtx
               
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

=======
                        postList tags ("posts/*" .&&. hasNoVersion) recentFirst
                    else let items = fromMaybe [] $ lookup related (tagsMap tags)
                         in postList tags (fromList items) recentFirst
            let relatedCtx = constField "related.title" title `mappend`
                             constField "related" list `mappend`
                             defaultContext
            let compiler = if bibFile /= "" then
                                bibtexCompiler cslFile bibFile
                           else pandocCompiler
            compiler
                >>= loadAndApplyTemplate "templates/related.html" relatedCtx
                >>= pageCompiler


    -- Project pages
    match "projects/**.md" $ do
        route $ setExtension ".html"
        compile $ pandocCompiler >>= pageCompiler

    match ("projects/**" .&&. complement "projects/**.md") $ do
        route   idRoute
        compile copyFileCompiler
>>>>>>> 77eb261fec70e8df3d52576c64865a4a8fc60730

    -- Post tags
    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged " ++ tag
        route idRoute
        compile $ do
            list <- postList tags pattern recentFirst
            makeItem ""
                >>= loadAndApplyTemplate "templates/post.html" (
                        mconcat [ constField "title" title
                                , constField "body" list
                                , defaultContext
                                ])
                >>= pageCompiler

    -- Render RSS feed
    create ["rss.xml"] $ do
        route idRoute
        compile $ loadAllSnapshots (allPostsPattern .&&. hasNoVersion) "content"
            >>= fmap (take 10) . recentFirst
            >>= renderRss feedConfiguration feedCtx

    create ["atom.xml"] $ do
        route idRoute
        compile $ loadAllSnapshots (allPostsPattern .&&. hasNoVersion) "content"
            >>= fmap (take 10) . recentFirst
            >>= renderAtom feedConfiguration feedCtx

    create ["sitemap.xml"] $ do
        route idRoute
        compile $ do
<<<<<<< HEAD
            posts <- loadAll (("pages/*" .||. allPostsPattern.||. "projects/**.md") .&&. hasNoVersion)
=======
            posts <- loadAll (("pages/*" .||. "posts/*" .||. "projects/**.md")
                              .&&. hasNoVersion)
>>>>>>> 77eb261fec70e8df3d52576c64865a4a8fc60730
            itemTpl <- loadBody "templates/sitemap-item.xml"
            list <- applyTemplateList itemTpl (sitemapCtx feedConfiguration) posts
            makeItem list
                >>= loadAndApplyTemplate "templates/sitemap.xml" defaultContext

    match "assets/txt/*" $ do
        route $ gsubRoute "assets/txt/" (const "")
        compile copyFileCompiler

    -- Read templates
    match "templates/*" $ compile templateCompiler

<<<<<<< HEAD
postCtx2 :: Context String
postCtx2 = mconcat [ dateField "date.machine" (iso8601DateFormat Nothing)
                  , dateField "date" "%B %e, %Y"
                  , field "footnotes" $ \item ->
                        loadBody ((itemIdentifier item) { identifierVersion = Just "footnotes"})
                  , field "toc" $ \item ->
                        loadBody ((itemIdentifier item) { identifierVersion = Just "toc"})
                  , modificationTimeField "updated.machine" (iso8601DateFormat Nothing)
                  , modificationTimeField "updated" "%B %e, %Y"
                  , dateField "date.day" "%d"
                  , dateField "date.month" "%b"
                  , dateField "date.year" "%Y"
                  --, defaultContext
                  , constField "title" "hello title postCtx2"
                  , constField "body" "hello body postCtx2"
                  , constField "author" "hello author postCtx2"
                  ]

=======
-- Auxiliary compilers
pageCompiler :: Item String -> Compiler (Item String)
pageCompiler i = loadAndApplyTemplate "templates/default.html" defaultContext i
               >>= relativizeUrls

bibtexCompiler :: String -> String -> Compiler (Item String)
bibtexCompiler cslFileName bibFileName = do 
    csl <- load (fromFilePath $ "assets/csl/"++cslFileName)
    bib <- load (fromFilePath $ "assets/bib/"++bibFileName)
    liftM writePandoc
        (getResourceBody >>= readPandocBiblio def (Just csl) bib)

postList :: Tags -> Pattern -> ([Item String] -> Compiler [Item String])
         -> Compiler String
postList tags pattern preprocess' = do
    postItemTpl <- loadBody "templates/archive-item.html"
    posts <- preprocess' =<< loadAll (pattern .&&. hasNoVersion)
    applyTemplateList postItemTpl (tagsCtx tags) posts

-- Contexts
>>>>>>> 77eb261fec70e8df3d52576c64865a4a8fc60730
postCtx :: Context String
postCtx = mconcat [ dateField "date.machine" (iso8601DateFormat Nothing)
                  , dateField "date" "%B %e, %Y"
                  , field "footnotes" $ \item ->
                        loadBody ((itemIdentifier item) { identifierVersion = Just "footnotes"})
                  , field "toc" $ \item ->
                        loadBody ((itemIdentifier item) { identifierVersion = Just "toc"})
                  , modificationTimeField "updated.machine" (iso8601DateFormat Nothing)
                  , modificationTimeField "updated" "%B %e, %Y"
                  , dateField "date.day" "%d"
                  , dateField "date.month" "%b"
                  , dateField "date.year" "%Y"
                  , defaultContext
                  ]


homeCtx :: Tags -> String -> Context String
homeCtx tags list =
    constField "posts" list `mappend`
    constField "title" "Index" `mappend`
    field "taglist" (\_ -> renderTagList tags) `mappend`
    postCtx

feedCtx :: Context String
feedCtx = mconcat [ postCtx
                  , metadataField
                  ]

tagsCtx :: Tags -> Context String
tagsCtx tags = mconcat [ tagsField "prettytags" tags
                       , postCtx
                       ]

sitemapCtx :: FeedConfiguration -> Context String
sitemapCtx conf = mconcat [ constField "root" (feedRoot conf)
                          , feedCtx
                          ]

-- Configuration
config :: Configuration
config = defaultConfiguration

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle = "henceworth - RSS feed"
    , feedDescription = "Latest articles from Henceworth"
    , feedAuthorName = "Hence Worth"
    , feedAuthorEmail = "info@henceworth.org"
    , feedRoot = "http://henceworth.org"
    }

<<<<<<< HEAD
postList :: Tags -> Pattern -> ([Item String] -> Compiler [Item String])
         -> Compiler String
postList tags pattern preprocess' = do
    postItemTpl <- loadBody "templates/archive-item.html"
    posts <- preprocess' =<< loadAll (pattern .&&. hasNoVersion)
    applyTemplateList postItemTpl (tagsCtx tags) posts

-- extractNotes, with the help of John MacFarlane
extractNotes :: Pandoc -> Pandoc 
extractNotes (Pandoc meta blocks) = Pandoc meta blocks' 
    where blocks' = queryWith getNoteContents blocks 

getNoteContents :: Inline -> [Block] 
getNoteContents (Note bs) = bs 
getNoteContents _         = [] 

removeNotes :: Pandoc -> Pandoc
removeNotes = bottomUp go
    where go (Note _) = Str ""
          go x = x

=======
>>>>>>> 77eb261fec70e8df3d52576c64865a4a8fc60730
