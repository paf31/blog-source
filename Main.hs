{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Char
import Data.String
import Data.Monoid
import Data.Maybe
import Data.List (intercalate, isSuffixOf, groupBy, nub, sort)
import Data.List.Split (splitOn, splitOneOf)
import Data.Time.Format as Time

import Control.Arrow ((***), (&&&))
import Control.Applicative ((<$>), (<*>))
import Network.URI as URI
import System.IO (hPutStr, hClose)
import System.FilePath (dropExtension, (</>))
import System.Time
import System.Directory

import Text.Blaze.Html
import Text.Blaze.Html.Renderer.String
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Text.RSS as RSS

import Cheapskate (markdown, def)
import Cheapskate.Html (renderDoc)
import Cheapskate.Types

data Post = Post
  { filename :: String
  , tags     :: [(String, String)]
  , content  :: String
  } deriving (Show, Eq)

copyright :: String
copyright = "© Phil Freeman 2010-2018"

readTags :: String -> ([(String, String)], [String])
readTags = readTags' False [] . splitOneOf "\n\r" where
  readTags' :: Bool -> [(String, String)] -> [String] -> ([(String, String)], [String])
  readTags' _ ts [] = (ts, [])
  readTags' False ts (('-':'-':'-':_):ss) = readTags' True ts ss
  readTags' True ts (('-':'-':'-':_):ss) = (ts, ss)
  readTags' False ts (_:ss) = readTags' False ts ss
  readTags' True ts (s:ss) = readTags' True (readTag s:ts) ss
  readTag :: String -> (String, String)
  readTag s = let (key:rest) = splitOn ":" s in (key, dropWhile isSpace $ intercalate ":" rest)

getAllPosts :: FilePath -> IO [Post]
getAllPosts path = do
  posts <- reverse . sort . filter (isSuffixOf ".markdown") <$> getDirectoryContents path
  flip mapM posts $ \filename -> do
    content <- readFile $ path </> filename
    let split = readTags content
    return $ Post
      { filename = filename
      , tags     = fst split
      , content  = intercalate "\n" $ snd split }

markdownToHtml :: String -> H.Html
markdownToHtml = renderDoc . markdown def { allowRawHtml = True, sanitize = False } . fromString

postFilename :: String -> String
postFilename = (++ ".html") . dropExtension

collectTags :: [Post] -> [(String, [Post])]
collectTags posts =
  map (id &&& postsFor posts) $ allTags posts
  where
  postsFor posts tag = filter (elem tag . tagsFor) posts
  tagsFor = map (dropWhile isSpace) . filter (not . null) . splitOneOf ",". fromJust . lookup "tags" . tags
  allTags = sort . nub . concatMap tagsFor

defaultTemplate :: String -> String -> String -> H.Html -> H.Html
defaultTemplate title subtitle rootPrefix body = do
  H.docType
  H.html $ do
    H.head $ do
      H.title $ H.toHtml $ "Functorial Blog - " ++ title
      H.link ! A.rel "stylesheet"
             ! A.type_ "text/css"
             ! A.href "//fonts.googleapis.com/css?family=Roboto+Slab:300"
      H.link ! A.rel "stylesheet"
             ! A.type_ "text/css"
             ! A.href "//fonts.googleapis.com/css?family=Roboto+Mono:300"
      H.link ! A.rel "stylesheet"
             ! A.type_ "text/css"
             ! A.href (fromString $ rootPrefix </> "assets" </> "default.css")
      H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
      H.script ! A.type_ "text/javascript" ! A.src (fromString $ rootPrefix </> "assets" </> "gaq.js") $ mempty
    H.body $ do
      H.header $ do
        H.h1 $ fromString title
        H.p $ fromString subtitle
        H.hr
      H.main body
      H.footer $ do
        H.hr
        H.p ! A.class_ "text-muted" $ H.small $ fromString copyright

renderPost :: Post -> H.Html
renderPost Post{..} = do
  let title = maybe "" id $ lookup "title" tags
      author = maybe "" id $ lookup "author" tags
      date = maybe "" id $ lookup "date" tags
  defaultTemplate title ("by " ++ author ++ " on " ++ date) ".." (markdownToHtml content)

renderPostLink :: String -> Post -> H.Html
renderPostLink rootPrefix Post{..} = do
  let title = maybe "" id $ lookup "title" tags
      date = maybe "" id $ lookup "date" tags
  H.li $ do
    H.a ! A.href (fromString $ rootPrefix </> "posts/" </> postFilename filename) $ fromString title
    H.em ! A.class_ "text-muted" $ do
      fromString " ("
      fromString date
      fromString ")"

renderIndex :: [Post] -> H.Html
renderIndex posts = defaultTemplate "Functorial Blog" "A blog about functional programming" "./" $ do
  H.h2 "Posts"
  H.ul $ mapM_ (renderPostLink ".") posts
  H.p $ H.small $ H.a ! A.href "feed.rss" $ fromString "RSS Feed"

renderFeed :: [Post] -> RSS.RSS
renderFeed posts =
  RSS.RSS "Functorial Blog"
          (URI.URI
            "https:"
            (Just (URI.URIAuth "" "blog.functorial.com" ""))
            "/feed.rss"
            ""
            "")
          "A blog about functional programming"
          [ RSS.Language "English"
          , RSS.Copyright copyright
          , RSS.WebMaster "freeman.phil@gmail.com"
          ]
          (map renderFeedPost posts)

renderFeedPost :: Post -> RSS.Item
renderFeedPost post =
    [ RSS.Title title
    , RSS.Link $
        URI.URI
          "https:"
          (Just (URI.URIAuth "" "blog.functorial.com" ""))
          ("/posts/" ++ postFilename (filename post))
          ""
          ""
    , RSS.PubDate (Time.parseTimeOrError True Time.defaultTimeLocale "%Y/%m/%d" date)
    ]
  where
    title = maybe "" id $ lookup "title" (tags post)
    date = maybe "" id $ lookup "date" (tags post)

writeHtml :: FilePath -> H.Html -> IO ()
writeHtml fp = writeFile fp . renderHtml

writeRSS :: FilePath -> RSS.RSS -> IO ()
writeRSS fp = writeFile fp . RSS.showXML . RSS.rssToXML

main :: IO ()
main = do
  dir <- getCurrentDirectory
  let
    dirOutput  = dir </> "output"
    dirPosts   = dir </> "output" </> "posts"
    dirAssets  = dir </> "output" </> "assets"
  assets <- filter (\f -> isSuffixOf ".js" f || isSuffixOf ".css" f) <$> getDirectoryContents (dir </> "assets")
  posts <- getAllPosts $ dir </> "posts"
  mapM_ (createDirectoryIfMissing False) [ dirOutput, dirAssets, dirPosts ]
  mapM_ (\f -> copyFile (dir </> "assets" </> f) (dirAssets </> f)) assets
  mapM_ (\post -> writeHtml (dirPosts </> postFilename (filename post)) (renderPost post)) posts
  writeHtml (dirOutput </> "index.html") (renderIndex posts)
  writeRSS (dirOutput </> "feed.rss") (renderFeed posts)
