{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative hiding ((<|>), many)
import           Control.Arrow (first)
import qualified Control.Foldl as L
import           Control.Lens hiding (Context, pre)
import           Control.Monad hiding (forM_)
import           Control.Monad.Catch hiding (try)
import           Data.Attoparsec.Text hiding (take, takeWhile, match)
import           Data.Char
import           Data.Foldable hiding (elem)
import           Data.List hiding (concatMap, any, all)
import           Data.List.Split hiding (oneOf)
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
--import qualified Data.Text.Encoding as T
import           Data.Text.Lazy (unpack)
import           Data.Time
import           Hakyll
import           Pipes as P
--import           Pipes.Attoparsec as P
import qualified Pipes.Group as P
import qualified Pipes.Prelude as P
import           Pipes.Safe
import           Pipes.Shell
import qualified Pipes.Text as Text
import qualified Pipes.Text.IO as Text
import qualified Pipes.Text.Encoding as Text
import           Prelude hiding (concatMap, any, all)
import           System.Directory
import           System.FilePath
import           System.IO hiding (utf8)
import           System.IO.Unsafe (unsafePerformIO)
import           System.Process
import           Text.Blaze.Html ((!), toValue)
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Internal (preEscapedString)
import           Text.Pandoc (Block (CodeBlock), Pandoc, bottomUpM)
import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.Walk as Pandoc
import qualified Text.ParserCombinators.Parsec as Parsec

import Debug.Trace

main :: IO ()
main = hakyllWith config $ do
    -- Read templates
    match "templates/*" $ compile templateCompiler

    -- Static files
    match (     "images/*.jpg"
           .||. "images/*.png"
           .||. "images/*.gif"
           .||. "favicon.ico"
           .||. "files/**"
          ) $ do
        route   idRoute
        compile copyFileCompiler

    -- Render the 404 page, we don't relativize URL's here.
    match "404.html" $ do
        route idRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext

    -- Compress CSS
    match "css/*.css" $ do
        route idRoute
        compile yuiCompressor

    -- Compress and minify JavaScript
    match "js/*.js" $ do
        route idRoute
        compile yuiCompressor

    -- Compress and minify Blueprint CSS
    forM_ [ "blueprint-css/blueprint/*.css"
          , "blueprint-css/blueprint/plugins/fancy-type/*.css"
          ] $ \p -> match p $ do
        route idRoute
        compile yuiCompressor

    -- Build tags
    tags <- buildTags "posts/*" (fromCapture "label/*")

    -- Match all files under posts directory and its subdirectories.
    -- Turn posts into wordpress style url: year/month/title/index.html
    forM_ [("posts/*",
            "templates/post.html",
            "templates/postfooter.html"
           ),
           ("pages/*",
            "templates/page.html",
            "templates/pagefooter.html")] $ \(p, t, f) ->
        match p $ do
            route wordpressRoute
            compile $ do
                let allCtx =
                        field "recent" (const recentPostList) <>
                        defaultContext

                pandocCompiler
                    >>= saveSnapshot "teaser"
                    >>= loadAndApplyTemplate t (postCtx tags)
                    >>= saveSnapshot "content"
                    >>= loadAndApplyTemplate f (postCtx tags)
                    >>= loadAndApplyTemplate "templates/default.html" allCtx
                    >>= wordpressifyUrls

    -- Build special pages
    forM_ ["index.markdown", "404.markdown", "search.markdown"] $ \p ->
        match p $ do
            route   $ setExtension "html"
            compile $ do
                let allCtx = field "recent" (const recentPostList)
                                 <> defaultContext
                pandocCompiler
                    >>= loadAndApplyTemplate "templates/page.html"
                                             (postCtx tags)
                    >>= loadAndApplyTemplate "templates/default.html" allCtx
                    >>= wordpressifyUrls

    -- Labels
    tagsRules tags $ \tag pattern -> do
        let title = "Posts with label " ++ " &#8216;" ++ tag ++ "&#8217;"
        route labelRoute
        compile $ do
            let allCtx = field "recent" (const recentPostList) <>
                         defaultContext
            list <- postList tags pattern recentFirst
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html"
                        (constField "title" title <>
                         constField "posts" list  <> defaultContext)
                >>= loadAndApplyTemplate "templates/default.html" allCtx
                >>= wordpressifyUrls

    paginate 6 10 $ \idx maxIndex itemsForPage -> do
        let ident
                | show idx == "1" = fromFilePath "index.html"
                | otherwise = fromFilePath $
                    "blog/page/" ++ show idx ++ "/index.html"
        create [ident] $ do
            route idRoute
            compile $ do
                let allCtx = field "recent" (const recentPostList) <>
                             defaultContext
                    loadTeaser ident' = loadSnapshot ident' "teaser"
                        >>= loadAndApplyTemplate "templates/teaser.html"
                            (-- constField "title" "Lost in Technopolis" <>
                             teaserCtx tags)
                        >>= wordpressifyUrls
                items  <- mapM loadTeaser itemsForPage
                let postsCtx =
                        constField "posts" (concatMap itemBody items) <>
                        field "navlinkolder"
                          (const $ return $ indexNavLink idx 1 maxIndex) <>
                        field "navlinknewer"
                          (const $ return $ indexNavLink idx (-1) maxIndex) <>
                        defaultContext

                makeItem ""
                    >>= loadAndApplyTemplate "templates/blogpage.html" postsCtx
                    >>= loadAndApplyTemplate "templates/default.html" allCtx
                    >>= wordpressifyUrls

    -- Render RSS feed
    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots "posts/*" "content"
            renderRss feedConfiguration feedContext posts

postCtx :: Tags -> Context String
postCtx tags = mconcat
    [ dateField "date" "%B %e, %Y"
    , tagsField "tags" tags
    , defaultContext
    ]

teaserCtx :: Tags -> Context String
teaserCtx tags = field "teaser" teaserBody <> postCtx tags

teaserBody :: Item String -> Compiler String
teaserBody item = do
    let body = itemBody item
    return $ extractTeaser . maxLengthTeaser . compactTeaser $ body
  where
    extractTeaser :: String -> String
    extractTeaser [] = []
    extractTeaser xs@(x : xr)
        | "<!--more-->" `isPrefixOf` xs = []
        | otherwise = x : extractTeaser xr

    maxLengthTeaser :: String -> String
    maxLengthTeaser s
        | isNothing (findIndex (isPrefixOf "<!--more-->") (tails s))
            = unwords (take 60 (words s))
        | otherwise = s

    compactTeaser :: String -> String
    compactTeaser
        = replaceAll "<iframe [^>]*>" (const "")
        . replaceAll "<img [^>]*>" (const "")
        . replaceAll "<p>" (const "")
        . replaceAll "</p>" (const "")
        . replaceAll "<blockquote>" (const "")
        . replaceAll "</blockquote>" (const "")
        . replaceAll "<strong>" (const "")
        . replaceAll "</strong>" (const "")
        . replaceAll "<ol>" (const "")
        . replaceAll "</ol>" (const "")
        . replaceAll "<ul>" (const "")
        . replaceAll "</ul>" (const "")
        . replaceAll "<li>" (const "")
        . replaceAll "</li>" (const "")
        . replaceAll "<h[0-9][^>]*>" (const "")
        . replaceAll "</h[0-9]>" (const "")
        . replaceAll "<pre.*" (const "")
        . replaceAll "<a [^>]*>" (const "")
        . replaceAll "</a>" (const "")

config :: Configuration
config = defaultConfiguration { deployCommand = "./deploy" }

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "What Thoughts May Come"
    , feedDescription = "RSS feed for John Wiegley's blog"
    , feedAuthorName  = "John Wiegley"
    , feedAuthorEmail = "jwiegley@gmail.com"
    , feedRoot        = "http://johnwiegley.com"
    }

labelRoute :: Routes
labelRoute = setExtension ".html"
    `composeRoutes` gsubRoute "." adjustLink
    `composeRoutes` gsubRoute "/" (const "")
    `composeRoutes` gsubRoute "^label" (const "label/")
    `composeRoutes` gsubRoute "-html" (const "/index.html")

adjustLink :: String -> String
adjustLink = filter (not . isSlash) . map (toLower . replaceWithDash)

replaceWithDash :: Char -> Char
replaceWithDash c = if c == '.' || c == ' ' then '-' else c

isSlash :: Char -> Bool
isSlash = (==) '/'

wordpressRoute :: Routes
wordpressRoute = gsubRoute "posts/" (const "")
    `composeRoutes` gsubRoute "pages/" (const "")
    `composeRoutes` gsubRoute "^[0-9]{4}-[0-9]{2}-[0-9]{2}-"
        ((\x -> take 8 x ++ drop 11 x) . map replaceWithSlash)
    `composeRoutes` gsubRoute ".org" (const "/index.html")
    `composeRoutes` gsubRoute ".md" (const "/index.html")
  where
    replaceWithSlash c = if c == '-' || c == '_' then '/' else c

wordpressifyUrls :: Item String -> Compiler (Item String)
wordpressifyUrls item = do
    rt <- getRoute $ itemIdentifier item
    return $ case rt of
        Nothing -> item
        Just _  -> fmap wordpressifyUrlsWith item

wordpressifyUrlsWith :: String  -- ^ HTML to wordpressify
                     -> String  -- ^ Resulting HTML
wordpressifyUrlsWith = withUrls convert
  where
    convert = replaceAll "/index.html" (const "/")

-- | Replace a sublist with another list.
replace :: (Eq a) => [a] -> [a] -> [a] -> [a]
replace [] _ _ = []
replace l@(x:xs) i j = if i `isPrefixOf` l
                       then j ++ replace (drop (length i) l) i j
                       else x : replace xs i j

toWordPressUrl :: FilePath -> String
toWordPressUrl url =
    replaceAll "/index.html" (const "/") (toUrl url)

wpUrlField :: String -> Context a
wpUrlField key = field key $
    fmap (maybe "" toWordPressUrl) . getRoute . itemIdentifier

feedContext :: Context String
feedContext = mconcat
    [ rssBodyField "description"
    , rssTitleField "title"
    , wpUrlField "url"
    , dateField "date" "%B %e, %Y"
    ]

rssTitleField :: String -> Context a
rssTitleField key = field key $ \i -> do
    value <- getMetadataField (itemIdentifier i) "title"
    let value' = liftM (replaceAll "&" (const "&amp;")) value
    maybe empty return value'

rssBodyField :: String -> Context String
rssBodyField key =
    field key
        $ return
        . replaceAll "<iframe [^>]*>" (const "")
        . withUrls wordpress
        . withUrls absolute
        . itemBody
  where
    wordpress = replaceAll "/index.html" (const "/")

    absolute x = if head x == '/'
                 then feedRoot feedConfiguration ++ x
                 else x

postList :: Tags -> Pattern -> ([Item String] -> Compiler [Item String])
         -> Compiler String
postList tags pattern preprocess' = do
    itemTpl <- loadBody "templates/postitem.html"
    posts   <- preprocess' =<< loadAll (pattern .&&. hasNoVersion)
    applyTemplateList itemTpl (postCtx tags) posts

recentPostList :: Compiler String
recentPostList = do
    posts   <- fmap (take 6) . recentFirst =<< recentPosts
    itemTpl <- loadBody "templates/indexpostitem.html"
    applyTemplateList itemTpl defaultContext posts

recentPosts :: Compiler [Item String]
recentPosts = do
    identifiers <- getMatches "posts/*"
    return [Item identifier "" | identifier <- identifiers]

indexNavLink :: Int -> Int -> Int -> String
indexNavLink n d maxn = renderHtml ref
  where
    ref = if refPage == ""
          then ""
          else H.a ! A.href (toValue $ toUrl refPage) $ preEscapedString lab
    lab = if d > 0
          then "Older Entries &raquo;"
          else "&laquo; Newer Entries"
    refPage = if n + d < 1 || n + d > maxn
              then ""
              else case n + d of
                1 -> "/"
                _ -> "/blog/page/" ++ show (n + d) ++ "/"

paginate:: Int -> Int -> (Int -> Int -> [Identifier] -> Rules ()) -> Rules ()
paginate itemsPerPage pageLimit rules = do
    identifiers <- getMatches "posts/*"
    let sorted      = sortBy (flip byDate) identifiers
        chunks      = take pageLimit $ chunksOf itemsPerPage sorted
        maxIndex    = length chunks
        pageNumbers = take maxIndex [1..]
        process i   = rules i maxIndex
    zipWithM_ process pageNumbers chunks
  where
    byDate id1 id2 =
        let fn1 = takeFileName (toFilePath id1)
            fn2 = takeFileName (toFilePath id2)
            parseTime' fn
                = parseTimeM False defaultTimeLocale "%Y-%m-%d"
                $ intercalate "-"
                $ take 3
                $ splitAll "-" fn
        in compare (parseTime' fn1 :: Maybe UTCTime)
                   (parseTime' fn2 :: Maybe UTCTime)

yuiCompressor :: Compiler (Item String)
yuiCompressor = do
    path <- getResourceFilePath
    makeItem $ unsafePerformIO $ do
        home <- getHomeDirectory
        let javaCmd = "java -jar "
                   ++ (home </> ".nix-profile/lib/yuicompressor.jar")
                   ++ " "
                   ++ path
        -- Where there is no decoding failure, the return value of the text
        -- stream will be an empty byte stream followed by its own return
        -- value.  In all cases you must deal with the fact that it is a
        -- 'ByteString' producer that is returned, even if it can be thrown
        -- away with 'Control.Monad.void'
        runSafeT $
            unpack <$> Text.toLazyM (void (producerCmd' javaCmd ^. Text.utf8))
