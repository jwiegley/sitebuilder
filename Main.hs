{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Applicative
import Control.Monad hiding (forM_)
import Data.Aeson (FromJSON (..), Value (Object), (.:))
import qualified Data.Aeson.Key as AT
import qualified Data.Aeson.KeyMap as AT
import Data.Aeson.Types (typeMismatch)
import qualified Data.Aeson.Types as AT
import Data.Char (toLower)
import Data.Foldable hiding (elem)
import Data.Functor
import Data.Generics (everywhereM, mkM)
import Data.List hiding (all, any, concatMap)
import Data.List.Split hiding (oneOf)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
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
import Text.Show.Pretty
import Prelude hiding (all, any, concatMap)

($$=) :: Identifier -> Context a -> Item a -> Compiler (Item String)
($$=) = loadAndApplyTemplate

yuiCompressor :: Compiler (Item String)
yuiCompressor = do
  path <- getResourceFilePath
  makeItem $ unsafePerformIO $ readProcess "yuicompressor" [path] ""

main :: IO ()
main = do
  now <- getCurrentTime

  let config =
        defaultConfiguration
          { provideMetadata = pandocMetadata
          }

  hakyllWith config $ do
    match "templates/*.html" $
      compile templateCompiler

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

    match "pages/*.org" $ do
      route $ metadataRoute (constRoute . getRouteFromMeta)
      compile $
        postPandocCompiler "pages"
          >>= "templates/page.html" $$= defaultContext
          >>= loadForSite

    tags <-
      buildTagsWith
        (getTagsByField "tags")
        "posts/*.org"
        (fromCapture "tags/*/index.html")

    posts <- getMatchesBefore now "posts/*.org"
    create posts $ do
      route $ metadataRoute (constRoute . getRouteFromMeta)
      compile $
        postPandocCompiler "posts"
          >>= saveSnapshot "teaser"
          >>= "templates/post.html"
            $$= (tagsField "tags" tags <> postCtxWithTags tags)
          >>= saveSnapshot "content"
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
                    <> listField "posts" (postCtxWithTags tags) (return ps)
                    <> listField
                      "tags"
                      (postCtxWithTags tags)
                      ( return $
                          map
                            (\x -> Item (fromFilePath (fst x)) (fst x))
                            (tagsMap tags)
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
                        (field "teaser" teaserBody <> postCtxWithTags tags)
                        ( forM itemsForPage $ \ident' ->
                            loadSnapshot ident' "teaser"
                              >>= wordpressifyUrls
                              >>= relativizeUrls
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
                      (postCtxWithTags tags)
                      ( forM itemsForPage $ \ident' ->
                          loadSnapshot ident' "teaser"
                            >>= wordpressifyUrls
                            >>= relativizeUrls
                      )
                      <> listField
                        "tags"
                        (postCtxWithTags tags)
                        ( return $
                            map (\x -> Item (fromFilePath (fst x)) (fst x)) $
                              tagsMap tags
                        )
                      <> defaultContext
                  )
            >>= loadForSite

    create ["atom.xml"] $ do
      route idRoute
      compile $ do
        renderAtom feedConfiguration (postCtxWithTags tags <> feedContext)
          =<< return . take 10
          =<< recentFirst
          =<< traverse (`loadSnapshot` "content") posts

    create ["rss.xml"] $ do
      route idRoute
      compile $
        renderRss feedConfiguration (postCtxWithTags tags <> feedContext)
          =<< return . take 10
          =<< recentFirst
          =<< traverse (`loadSnapshot` "content") posts

    create ["sitemap.xml"] $ do
      route idRoute
      compile $ do
        ps <- recentFirst =<< traverse load posts
        pages <- loadAll "pages/*.org"
        makeItem ("" :: String)
          >>= "templates/sitemap.xml"
            $$= ( listField
                    "entries"
                    postCtx
                    (return $ ps ++ pages)
                )
          >>= wordpressifyUrls
          >>= relativizeUrls
  where
    loadForSite =
      "templates/meta.html" $$= defaultContext
        >=> wordpressifyUrls
        >=> relativizeUrls

    postCtxWithTags :: Tags -> Context String
    postCtxWithTags tags = tagsField "tags" tags <> postCtx

    postCtx :: Context String
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

    postPandocCompiler :: FilePath -> Compiler (Item String)
    postPandocCompiler dir =
      pandocCompilerWithTransformM
        defaultHakyllReaderOptions
        defaultHakyllWriterOptions
        (unsafeCompiler . fixPostLinks)
      where
        fixPostLinks :: P.Pandoc -> IO P.Pandoc
        fixPostLinks = everywhereM (mkM fixPostLink)

        fixPostLink :: P.Inline -> IO P.Inline
        fixPostLink l@(P.Link as title (T.unpack -> url, title'))
          | AllTextSubmatches [_, uuid] <- (url =~ ("^id:(.+)$" :: String)) = do
              findPostByUuid dir uuid <&> \case
                Nothing -> l
                Just path ->
                  P.Link
                    as
                    title
                    (T.pack ("/" ++ path), title')
        fixPostLink x = return x

{------------------------------------------------------------------------}
-- Content normalization

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

{------------------------------------------------------------------------}
-- Sorting and pagination

itemUTC :: Identifier -> UTCTime
itemUTC ident =
  fromMaybe err $
    parseTime "%Y%m%d" (take 8 (takeBaseName (toFilePath ident)))
  where
    err =
      error $
        "itemUTC: "
          ++ "Could not parse time from filename "
          ++ show ident
    parseTime = parseTimeM True defaultTimeLocale

getMatchesBefore :: MonadMetadata m => UTCTime -> Pattern -> m [Identifier]
getMatchesBefore moment pat = filter dateBefore <$> getMatches pat
  where
    dateBefore ident = diffUTCTime moment (itemUTC ident) > 0

paginate ::
  UTCTime ->
  Maybe (Int, Int) ->
  (Int -> Int -> [Identifier] -> Rules ()) ->
  Rules ()
paginate moment mlim rules = do
  idents <- getMatchesBefore moment "posts/*.org"
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
    byDate id1 id2 = compare (itemUTC id1) (itemUTC id2)

{------------------------------------------------------------------------}
-- RSS/Atom feed

instance FromJSON FeedConfiguration where
  parseJSON (Object v) =
    FeedConfiguration
      <$> v .: "title"
      <*> v .: "description"
      <*> v .: "authorName"
      <*> v .: "authorEmail"
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
    [ rssTitleField "title",
      rssBodyField "description"
    ]

rssTitleField :: String -> Context String
rssTitleField key = field key $ \i -> do
  value <- getMetadataField (itemIdentifier i) "title"
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

    absolute x
      | head x == '/' = feedRoot feedConfiguration ++ x
      | otherwise = x

{------------------------------------------------------------------------}
-- Metadata

pandocMetadata :: FilePath -> IO Metadata
pandocMetadata file = do
  P.Pandoc (P.Meta meta) blocks <- do
    cnt <- TIO.readFile file
    case P.runPure $ P.readOrg P.def cnt of
      Right t -> return t
      Left e -> error $ "Pandoc read failed: " ++ show e
  let furtherMetadata =
        M.fromList $
          mapMaybe
            ( \b -> case b of
                P.RawBlock (P.Format "org") (T.unpack -> text) ->
                  case text =~ ("#\\+([A-Za-z]+):[ \t]+(.+)" :: String) of
                    AllTextSubmatches [_, key, value] ->
                      Just (T.pack key, P.MetaString (T.pack value))
                    _ -> Nothing
                _ -> Nothing
            )
            blocks
      -- The 'Semigroup' operation for 'Map' is 'union', which prefers values
      -- from the left operand.
      metadata = cleanupMetadata (meta <> furtherMetadata)
      result = buildMetadata (P.Meta metadata)
  -- putStrLn $ "result = " ++ ppShow result
  pure result

buildMetadata :: P.Meta -> Metadata
buildMetadata meta@(P.Meta metadata) =
  AT.fromList $
    map (\(f, t) -> (AT.fromString f, AT.String t)) $
      filter (not . T.null . snd) $
        map (\(f, ex, wr) -> (f, inlinesTo wr (ex meta))) $
          [ ("published", publishDateOrDocDate, P.writePlain),
            ("route", publishRoute, P.writePlain)
          ]
            ++ M.foldMapWithKey
              (\k _ -> [(T.unpack k, metaField k, P.writePlain)])
              metadata

cleanupMetadata :: M.Map T.Text P.MetaValue -> M.Map T.Text P.MetaValue
cleanupMetadata meta = M.foldMapWithKey ((M.fromList .) . go) meta
  where
    fixTitle (T.unpack -> value) =
      [ ( "title",
          P.MetaString . T.pack $
            case value =~ ("\"(.+)\"" :: String) of
              AllTextSubmatches [_, content] -> content
              _ -> value
        )
      ]

    go "title" (P.MetaString value) = fixTitle value
    go "title" (P.MetaInlines ils) = fixTitle (inlinesTo P.writePlain ils)
    go "filetags" (P.MetaString value) =
      [ ( "tags",
          P.MetaString
            ( T.intercalate ", "
                . filter
                  ( \(T.unpack -> s) ->
                      not (s =~ ("^publish=" :: String))
                  )
                . filter (not . T.null)
                . T.splitOn ":"
                $ value
            )
        )
      ]
    go key value = [(key, value)]

publishRoute :: P.Meta -> [P.Inline]
publishRoute meta =
  datePath
    ++ slugPath
    ++ [P.Str "index.html"]
  where
    slugPath = metaField "slug" meta ++ [P.Str "/"]
    datePath =
      case T.unpack (stringify (publishDate meta))
        =~ ("([0-9]+)-([0-9]+)-" :: String) of
        AllTextSubmatches [_, year, month] ->
          [ P.Str (T.pack year),
            P.Str "/",
            P.Str (T.pack month),
            P.Str "/"
          ]
        _ -> []

getRouteFromMeta :: Metadata -> FilePath
getRouteFromMeta meta =
  case lookupString "route" meta of
    Nothing -> error $ "missing route: " ++ show meta
    Just rte -> rte

publishDateOrDocDate :: P.Meta -> [P.Inline]
publishDateOrDocDate meta =
  case publishDate meta of
    [] -> P.docDate meta
    xs -> xs

publishDate :: P.Meta -> [P.Inline]
publishDate meta =
  case metaField "publish" meta of
    [P.Str s] | Just date <- orgDateToIso s -> [P.Str date]
    _ ->
      case metaField "created" meta of
        [P.Str s] | Just date <- orgDateToIso s -> [P.Str date]
        _ -> []

findPostByUuid :: FilePath -> String -> IO (Maybe FilePath)
findPostByUuid dir (map toLower -> uuid) = do
  posts <- getDirectoryContents dir
  results <- forM posts $ \post -> do
    if takeExtension post == ".org"
      then do
        meta <- pandocMetadata (dir </> post)
        pure $ case lookupString "id" meta of
          Just (map toLower -> postUuid)
            | uuid == postUuid ->
                maybeToList (lookupString "route" meta)
          _ -> []
      else pure []
  pure $ listToMaybe (concat results)

metaField :: T.Text -> P.Meta -> [P.Inline]
metaField name meta =
  case P.lookupMeta name meta of
    Just (P.MetaString s) -> [P.Str s]
    Just (P.MetaInlines ils) -> ils
    Just (P.MetaBlocks [P.Plain ils]) -> ils
    Just (P.MetaBlocks [P.Para ils]) -> ils
    _ -> []

inlinesTo ::
  (P.WriterOptions -> P.Pandoc -> P.PandocPure T.Text) ->
  [P.Inline] ->
  T.Text
inlinesTo wr ill =
  case P.runPure . wr P.def $ doc of
    Right t -> T.strip t
    Left e -> error $ "Pandoc write failed: " ++ show e
  where
    doc = P.Pandoc P.nullMeta [P.Plain ill]

stringify :: [P.Inline] -> T.Text
stringify = inlinesTo P.writePlain

-- Maybe convert an Org date of form [YYYY-MM-DD WWW HH:MM] to a date of the
-- form YYYY-MM-DD HH:MM:00.
orgDateToIso :: T.Text -> Maybe T.Text
orgDateToIso (T.unpack -> date) =
  case date
    =~ ( "\\[([0-9]+)-([0-9]+)-([0-9]+) [A-Za-z]+( [0-9:]+)?\\]" ::
           String
       ) of
    AllTextSubmatches [_, year, month, day, time] ->
      Just $ T.pack $ mconcat [year, "-", month, "-", day, time, ":00"]
    _ -> Nothing
