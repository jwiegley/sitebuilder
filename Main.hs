{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Applicative
import Control.Monad hiding (forM_)
import Data.Aeson (FromJSON(..), Value(Object), (.:))
import Data.Aeson.Types (typeMismatch)
import Data.Foldable hiding (elem)
import Data.List hiding (concatMap, any, all)
import Data.List.Split hiding (oneOf)
import Data.Maybe
import Data.Monoid
import Data.Time
import Data.Yaml (decodeFile)
import Hakyll
import Prelude hiding (concatMap, any, all)
import System.FilePath
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcess)

main :: IO ()
main = do
  now <- getCurrentTime

  -- directories: css images js pages posts templates

  hakyllWith defaultConfiguration $ do
    match "templates/*" $ compile templateCompiler

    match ("files/**"     .||.
           "images/*.jpg" .||.
           "images/*.png" .||.
           "images/*.gif" .||.
           "favicon.ico") $
        route idRoute >> compile copyFileCompiler

    match ("css/*.css" .||. "js/*.js") $
        route idRoute >> compile yuiCompressor

    match "pages/*" $ do
        route wordpressRoute
        compile $ pandocCompiler
            >>= "templates/page.html" $$= defaultContext
            >>= loadForSite

    matchMetadata allPosts (dateBefore now) $ do
        route wordpressRoute
        compile $ pandocCompiler
            >>= saveSnapshot "teaser"
            >>= "templates/post.html" $$= postCtx
            >>= saveSnapshot "content"
            >>= loadForSite

    paginate now (Just (6, 10)) $ \idx maxIndex itemsForPage ->
        create [fromFilePath $
                    if idx == 1
                    then "index.html"
                    else "page/" ++ show idx ++ "/index.html"] $ do
            route idRoute
            compile $ makeItem ""
                >>= "templates/list.html" $$=
                    (listField "posts"
                         (field "teaser" teaserBody <> postCtx)
                         (forM itemsForPage $ \ident' ->
                              loadSnapshot ident' "teaser"
                                  >>= wordpressifyUrls) <>
                     (if idx == 1
                      then constField "isFirst" "true"
                      else mempty) <>
                     (if idx == 2
                      then constField "isSecond" "true"
                      else mempty) <>
                     (if idx == maxIndex
                      then constField "isLast" "true"
                      else mempty) <>
                     constField "nextIndex" (show (succ idx)) <>
                     constField "prevIndex" (show (pred idx)) <>
                     defaultContext)
                >>= loadForSite

    paginate now Nothing $ \_ _ itemsForPage ->
        create [fromFilePath "archives/index.html"] $ do
            route idRoute
            compile $ makeItem ""
                >>= "templates/archives.html" $$=
                    (listField "posts" postCtx
                         (forM itemsForPage $ \ident' ->
                              loadSnapshot ident' "teaser"
                                  >>= wordpressifyUrls) <>
                     defaultContext)
                >>= loadForSite

    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            posts <- take 10 <$>
                (recentFirst =<< loadAllSnapshots allPosts "content")
            renderRss feedConfiguration feedContext posts

    create ["sitemap.xml"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll allPosts
            pages <- loadAll "pages/*"
            let allEntries = return (pages ++ posts)
            let sitemapCtx = mconcat
                    [ listField "entries" postCtx allEntries
                    , defaultContext
                    ]
            makeItem ""
                >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx
                >>= wordpressifyUrls
  where
    ($$=) = loadAndApplyTemplate

    loadForSite =
        "templates/meta.html" $$= defaultContext >=> wordpressifyUrls

    postCtx = mconcat
        [ dateField "date"  "%B %e, %Y"
        , dateField "year"  "%Y"
        , dateField "mon"   "%m"
        , dateField "month" "%B"
        , dateField "day_"  "%d"
        , dateField "day"   "%e"
        , wpIdentField "ident"
        , wpUrlField "url"
        , defaultContext
        ]

allPosts :: Pattern
allPosts = "posts/*"

yuiCompressor :: Compiler (Item String)
yuiCompressor = do
    path <- getResourceFilePath
    makeItem $ unsafePerformIO $ readProcess "yui-compressor" [path] ""

dateBefore :: UTCTime -> Metadata -> Bool
dateBefore moment meta = case lookupString "date" meta of
    Nothing -> False
    Just dateStr ->
        let mres = parseDate "%Y-%m-%d %a" dateStr    <|>
                   parseDate "%Y-%m-%d %H:%M" dateStr <|>
                   parseDate "%Y-%m-%d" dateStr       <|>
                   parseDate "%a, %d %b %Y %H:%M:%S %z" dateStr in
        case mres of
            Nothing -> False
            Just date -> diffUTCTime moment date > 0
  where
    parseDate = parseTimeM True defaultTimeLocale

teaserBody :: Item String -> Compiler String
teaserBody = return . extractTeaser . maxLengthTeaser . compactTeaser . itemBody
  where
    extractTeaser [] = []
    extractTeaser xs@(x : xr)
        | "<!--more-->" `isPrefixOf` xs = []
        | otherwise = x : extractTeaser xr

    maxLengthTeaser s
        | isNothing (findIndex (isPrefixOf "<!--more-->") (tails s))
            = unwords (take 60 (words s))
        | otherwise = s

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
        . replaceAll "<pre[^>]*>" (const "")
        . replaceAll "</pre>" (const "")
        . replaceAll "<a [^>]*>" (const "")
        . replaceAll "</a>" (const "")

wordpressRoute :: Routes
wordpressRoute =
    gsubRoute "posts/" (const "") `composeRoutes`
    gsubRoute "pages/" (const "") `composeRoutes`
    gsubRoute "^[0-9]{4}-[0-9]{2}-[0-9]{2}-"
        ((\x -> take 8 x ++ drop 11 x) . map replaceWithSlash) `composeRoutes`
    gsubRoute "\\.org$" (const "/index.html") `composeRoutes`
    gsubRoute "\\.md$" (const "/index.html")
  where
    replaceWithSlash c = if c == '-' || c == '_' then '/' else c

wordpressifyUrls :: Item String -> Compiler (Item String)
wordpressifyUrls item = do
    rt <- getRoute (itemIdentifier item)
    return $ case rt of
        Nothing -> item
        Just _  -> wordpressifyUrlsWith <$> item
  where
    wordpressifyUrlsWith = withUrls $ replaceAll "/index.html" (const "/")

wpUrlField :: String -> Context String
wpUrlField key = field key $
    fmap (maybe "" toWordPressUrl) . getRoute . itemIdentifier
  where
    toWordPressUrl = replaceAll "/index.html" (const "/") . toUrl

wpIdentField :: String -> Context String
wpIdentField = mapContext (last . init . splitOn "/") . wpUrlField

paginate:: UTCTime -> Maybe (Int, Int) -> (Int -> Int -> [Identifier] -> Rules ()) -> Rules ()
paginate moment mlim rules = do
    idents <- (getMatches allPosts >>=) $ mapM $ \ident -> do
        meta <- getMetadata ident
        return [ident | dateBefore moment meta]
    let sorted      = sortBy (flip byDate) (concat idents)
        chunks      = case mlim of
            Just (itemsPerPage, pageLimit) ->
                take pageLimit $ chunksOf itemsPerPage sorted
            Nothing -> [sorted]
        maxIndex    = length chunks
        pageNumbers = take maxIndex [1..]
        process i   = rules i maxIndex
    zipWithM_ process pageNumbers chunks
  where
    byDate id1 id2 =
        let fn1 = takeFileName (toFilePath id1)
            fn2 = takeFileName (toFilePath id2)
            parseTime'
                = parseTimeM False defaultTimeLocale "%Y-%m-%d"
                . intercalate "-"
                . take 3
                . splitAll "-"
        in compare (parseTime' fn1 :: Maybe UTCTime)
                   (parseTime' fn2 :: Maybe UTCTime)

instance FromJSON FeedConfiguration where
    parseJSON (Object v) =
        FeedConfiguration
            <$> v .: "title"
            <*> v .: "description"
            <*> v .: "author-name"
            <*> v .: "author-email"
            <*> v .: "root"
    parseJSON invalid = typeMismatch "FeedConfiguration" invalid

feedConfiguration :: FeedConfiguration
feedConfiguration =
    fromMaybe (error "Could not open or parse config.yaml file")
        $ unsafePerformIO $ decodeFile "config.yaml"

feedContext :: Context String
feedContext = mconcat
    [ rssBodyField "description"
    , rssTitleField "title"
    , wpUrlField "url"
    , dateField "date" "%Y-%m-%d"
    , dateField "year" "%Y"
    , dateField "month" "%B"
    , dateField "day_" "%d"
    , dateField "day" "%e"
    ]

rssTitleField :: String -> Context a
rssTitleField key = field key $ \i -> do
    value <- getMetadataField (itemIdentifier i) "title"
    let value' = replaceAll "&" (const "&amp;") <$> value
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
