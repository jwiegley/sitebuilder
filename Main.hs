{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Applicative
import Control.Monad hiding (forM_)
import Data.Aeson (FromJSON (..), Value (Object), (.:))
import Data.Aeson.Types (typeMismatch)
import Data.Foldable hiding (elem)
import Data.Generics (everywhereM, mkM)
import Data.List hiding (all, any, concatMap)
import Data.List.Split hiding (oneOf)
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Data.Time
import Data.Yaml (decodeFileEither)
import Debug.Trace
import Hakyll
import System.Directory
import System.FilePath
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcess)
import qualified Text.Pandoc as P
import Text.Regex.Posix hiding (empty, match)
import Prelude hiding (all, any, concatMap)

main :: IO ()
main = do
  now <- getCurrentTime

  hakyllWith defaultConfiguration $ do
    match "templates/*" $ compile templateCompiler

    match
      ( "files/**"
          .||. "images/**"
          .||. "favicon.ico"
          .||. "robots.txt"
      )
      $ do
        route idRoute
        compile copyFileCompiler

    match ("css/*.css" .||. "js/*.js") $ do
      route idRoute
      compile yuiCompressor

    match "pages/*" $ do
      route wordpressRoute
      compile $
        myPandocCompiler
          >>= "templates/page.html" $$= defaultContext
          >>= relativizeUrls
          >>= loadForSite

    tags <-
      buildTagsWith
        (getTagsByField "filetags")
        allPosts
        (fromCapture "tags/*/index.html")

    posts <- getMatchesBefore now allPosts
    create posts $ do
      route wordpressRoute
      compile $
        myPandocCompiler
          >>= saveSnapshot "teaser"
          >>= "templates/post.html"
            $$= (tagsField "filetags" tags <> postCtx)
          >>= saveSnapshot "content"
          >>= relativizeUrls
          >>= loadForSite

    tagsRules tags $ \tag pat -> do
      route idRoute
      compile $ do
        ps <-
          recentFirst
            =<< traverse load
            =<< getMatchesBefore now pat
        makeItem ""
          >>= "templates/archives.html"
            $$= ( constField "title" ("Posts tagged \"" ++ tag ++ "\"")
                    <> listField "posts" postCtx (return ps)
                    <> listField
                      "filetags"
                      postCtx
                      ( return $
                          map (\x -> Item (fromFilePath (fst x)) (fst x)) $
                            tagsMap tags
                      )
                    <> defaultContext
                )
          >>= loadForSite

    paginate now (Just (6, 10)) $ \idx maxIndex itemsForPage ->
      create
        [ fromFilePath $
            if idx == 1
              then "index.html"
              else "page/" ++ show idx ++ "/index.html"
        ]
        $ do
          route idRoute
          compile $
            makeItem ""
              >>= "templates/list.html"
                $$= ( listField
                        "posts"
                        (field "teaser" teaserBody <> postCtx)
                        ( forM itemsForPage $ \ident' ->
                            loadSnapshot ident' "teaser"
                              >>= wordpressifyUrls
                        )
                        <> ( if idx == 1
                               then constField "isFirst" "true"
                               else mempty
                           )
                        <> ( if idx == 2
                               then constField "isSecond" "true"
                               else mempty
                           )
                        <> ( if idx == maxIndex
                               then constField "isLast" "true"
                               else mempty
                           )
                        <> constField "nextIndex" (show (succ idx))
                        <> constField "prevIndex" (show (pred idx))
                        <> defaultContext
                    )
              >>= loadForSite

    paginate now Nothing $ \_ _ itemsForPage ->
      create [fromFilePath "archives/index.html"] $ do
        route idRoute
        compile $
          makeItem ""
            >>= "templates/archives.html"
              $$= ( listField
                      "posts"
                      postCtx
                      ( forM itemsForPage $ \ident' ->
                          loadSnapshot ident' "teaser"
                            >>= wordpressifyUrls
                      )
                      <> listField
                        "filetags"
                        postCtx
                        ( return $
                            map (\x -> Item (fromFilePath (fst x)) (fst x)) $
                              tagsMap tags
                        )
                      <> defaultContext
                  )
            >>= loadForSite

    create ["rss.xml"] $ do
      route idRoute
      compile $
        renderRss feedConfiguration (postCtx <> feedContext)
          =<< return . take 10
          =<< recentFirst
          =<< traverse (`loadSnapshot` "content") posts

    create ["sitemap.xml"] $ do
      route idRoute
      compile $ do
        ps <- recentFirst =<< traverse load posts
        pages <- loadAll "pages/*"
        let allEntries = return (pages ++ ps)
        let sitemapCtx =
              mconcat
                [ listField "entries" postCtx allEntries,
                  defaultContext
                ]
        makeItem ""
          >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx
          >>= wordpressifyUrls
  where
    ($$=) = loadAndApplyTemplate

    loadForSite =
      "templates/meta.html" $$= defaultContext
        >=> wordpressifyUrls

    postCtx =
      mconcat
        [ dateField "date" "%B %e, %Y",
          dateField "year" "%Y",
          dateField "mon" "%m",
          dateField "month" "%B",
          dateField "day_" "%d",
          dateField "day" "%e",
          wpIdentField "ident",
          wpUrlField "url",
          defaultContext
        ]

myPandocCompiler :: Compiler (Item String)
myPandocCompiler =
  pandocCompilerWithTransformM
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions
    (unsafeCompiler . fixPostLinks)

fixPostLinks :: P.Pandoc -> IO P.Pandoc
fixPostLinks doc@(P.Pandoc (P.Meta meta) _blocks) =
  trace ("meta = " ++ show meta) $ everywhereM (mkM fixPostLink) doc

fixPostLink :: P.Inline -> IO P.Inline
-- fixPostLink l@(P.Link as inl (T.unpack -> url, title))
fixPostLink
  l@( P.Span
        (ident, ["spurious-link"], [("target", T.unpack -> url)])
        [P.Emph title]
      )
    | AllTextSubmatches [_, result] <-
        (url =~ ("^{% post_url (.+) %}$" :: String)) = do
        mp <- findPosts result
        case mp of
          Nothing -> return l
          Just p ->
            let url' =
                  if hasExtension p
                    then replaceExtension ("/" </> p) "html"
                    else "/" </> p </> "index.html"
             in return $
                  P.Link
                    (ident, [], [])
                    title
                    (T.pack url', stringify title)
    | otherwise = return l
fixPostLink x = return x

stringify :: [P.Inline] -> T.Text
stringify = P.queryWith go
  where
    go :: P.Inline -> T.Text
    go P.Space = " "
    go (P.Str x) = x
    go (P.Code _ x) = x
    go (P.Math _ x) = x
    go P.LineBreak = " "
    go _ = ""

findPosts :: String -> IO (Maybe FilePath)
findPosts f = do
  posts <- getDirectoryContents "posts"
  results <- forM posts $ \post ->
    pure $ case post =~ ("([0-9]+)-([0-9]+)-[0-9]+-" ++ f :: String) of
      AllTextSubmatches [_, year, month] ->
        [year ++ "/" ++ month ++ "/" ++ f]
      _ -> []
  pure $ listToMaybe (concat results)

yuiCompressor :: Compiler (Item String)
yuiCompressor = do
  path <- getResourceFilePath
  makeItem $ unsafePerformIO $ readProcess "yuicompressor" [path] ""

getItemUTC' :: Identifier -> UTCTime
getItemUTC' id' =
  fromMaybe empty' $
    msum
      [ parseTime' "%Y-%m-%d" $
          intercalate "-" $
            take 3 $
              splitAll "-" fnCand
        | fnCand <- reverse (splitDirectories (toFilePath id'))
      ]
  where
    empty' = error $ "getItemUTC': " ++ "could not parse time for " ++ show id'
    parseTime' = parseTimeM True defaultTimeLocale

teaserBody :: Item String -> Compiler String
teaserBody = return . extractTeaser . maxLengthTeaser . compactTeaser . itemBody
  where
    extractTeaser [] = []
    extractTeaser xs@(x : xr)
      | "<!--more-->" `isPrefixOf` xs = []
      | otherwise = x : extractTeaser xr

    maxLengthTeaser s
      | isNothing (findIndex (isPrefixOf "<!--more-->") (tails s)) =
          unwords (take 60 (words s))
      | otherwise = s

    compactTeaser =
      replaceAll "<iframe [^>]*>" (const "")
        . replaceAll "<img [^>]*>" (const "")
        . replaceAll "<p>" (const "")
        . replaceAll "</p>" (const "")
        . replaceAll "<br />" (const "<br/>")
        . replaceAll "<div class=\"line-block\">" (const "")
        . replaceAll "</div>" (const "")
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
  gsubRoute "posts/" (const "")
    `composeRoutes` gsubRoute "pages/" (const "")
    `composeRoutes` gsubRoute
      "^[0-9]{4}-[0-9]{2}-[0-9]{2}-"
      ((\x -> take 8 x ++ drop 11 x) . map replaceWithSlash)
    `composeRoutes` gsubRoute "\\.org$" (const "/index.html")
    `composeRoutes` gsubRoute "\\.md$" (const "/index.html")
  where
    replaceWithSlash c = if c == '-' || c == '_' then '/' else c

wordpressifyUrls :: Item String -> Compiler (Item String)
wordpressifyUrls item = do
  rt <- getRoute (itemIdentifier item)
  return $ case rt of
    Nothing -> item
    Just _ -> wordpressifyUrlsWith <$> item
  where
    wordpressifyUrlsWith = withUrls $ replaceAll "/index.html" (const "/")

wpUrlField :: String -> Context String
wpUrlField key =
  field key $
    fmap (maybe "" toWordPressUrl) . getRoute . itemIdentifier
  where
    toWordPressUrl = replaceAll "/index.html" (const "/") . toUrl

wpIdentField :: String -> Context String
wpIdentField = mapContext (last . init . splitOn "/") . wpUrlField

allPosts :: Pattern
allPosts = "posts/*"

getMatchesBefore :: MonadMetadata m => UTCTime -> Pattern -> m [Identifier]
getMatchesBefore moment pat = filter dateBefore <$> getMatches pat
  where
    dateBefore ident = diffUTCTime moment (getItemUTC' ident) > 0

paginate ::
  UTCTime ->
  Maybe (Int, Int) ->
  (Int -> Int -> [Identifier] -> Rules ()) ->
  Rules ()
paginate moment mlim rules = do
  idents <- getMatchesBefore moment allPosts
  let sorted = sortBy (flip byDate) idents
      chunks = case mlim of
        Just (itemsPerPage, pageLimit) ->
          take pageLimit $ chunksOf itemsPerPage sorted
        Nothing -> [sorted]
      maxIndex = length chunks
      pageNumbers = take maxIndex [1 ..]
      process i = rules i maxIndex
  zipWithM_ process pageNumbers chunks
  where
    byDate id1 id2 =
      let fn1 = takeFileName (toFilePath id1)
          fn2 = takeFileName (toFilePath id2)
          parseTime' =
            parseTimeM False defaultTimeLocale "%Y-%m-%d"
              . intercalate "-"
              . take 3
              . splitAll "-"
       in compare
            (parseTime' fn1 :: Maybe UTCTime)
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
  either (error "Could not open or parse config.yaml file") id $
    unsafePerformIO $
      decodeFileEither "config.yaml"

feedContext :: Context String
feedContext =
  mconcat
    [ rssBodyField "description"
    -- , rssTitleField "title"
    ]

rssTitleField :: String -> Context String
rssTitleField key = field key $ \i -> do
  traceM $ "itemIdentifier i = " ++ show (itemIdentifier i)
  value <- getMetadataField (itemIdentifier i) "title"
  traceM $ "title value = " ++ show value
  maybe empty return $ replaceAll "&" (const "&amp;") <$> value

rssBodyField :: String -> Context String
rssBodyField key =
  field key $
    return
      . replaceAll "<iframe [^>]*>" (const "")
      . withUrls wordpress
      . withUrls absolute
      . itemBody
  where
    wordpress = replaceAll "/index.html" (const "/")

    absolute x =
      if head x == '/'
        then feedRoot feedConfiguration ++ x
        else x
