{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Char
import Data.String
import Data.Monoid
import Data.Maybe
import Data.List (intercalate, isSuffixOf, groupBy, nub, sort)
import Data.List.Split (splitOn, splitOneOf)

import Control.Arrow ((***), (&&&))
import Control.Applicative ((<$>), (<*>))
import System.IO (hPutStr, hClose)
import System.FilePath (dropExtension, (</>))
import System.Time
import System.Directory

import Text.Blaze.Html
import Text.Blaze.Html.Renderer.String
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Cheapskate (markdown, def)
import Cheapskate.Html (renderDoc)
import Cheapskate.Types

data Post = Post
  { filename :: String
  , tags     :: [(String, String)]
  , content  :: String
  } deriving (Show, Eq)

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

defaultTemplate :: String -> String -> H.Html -> H.Html
defaultTemplate title rootPrefix body = do
  H.docType
  H.html $ do
    H.head $ do
      H.title $ H.toHtml $ "Functorial Blog - " ++ title
      H.link ! A.rel "stylesheet"
             ! A.type_ "text/css"
             ! A.href "http://fonts.googleapis.com/css?family=Roboto+Slab:400,300"
      H.link ! A.rel "stylesheet"
             ! A.type_ "text/css"
             ! A.href "http://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/css/bootstrap.min.css"
      H.link ! A.rel "stylesheet"
             ! A.type_ "text/css"
             ! A.href (fromString $ rootPrefix </> "assets" </> "default.css")
      H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
      H.script ! A.type_ "text/javascript" ! A.src (fromString $ rootPrefix </> "assets" </> "gaq.js") $ mempty
    H.body $
       H.div ! A.class_ "container" $ do
         H.section $ do
           H.h1 $ H.a ! A.href (fromString $ rootPrefix </> "index.html") $ fromString "Functorial Blog"
           H.p ! A.class_ "lead" $ fromString "A blog about functional programming"
           H.hr
         H.section body
         H.section $ do
           H.hr
           H.p ! A.class_ "text-muted" $ H.small $ fromString "Copyright Phil Freeman 2010-2016"

renderPost :: Post -> H.Html
renderPost Post{..} = do
  let title = maybe "" id $ lookup "title" tags
      author = maybe "" id $ lookup "author" tags
      date = maybe "" id $ lookup "date" tags
  defaultTemplate title ".." $ do
    H.h2 $ fromString title
    H.p $ H.small $ fromString $ "by " ++ author ++ " on " ++ date
    markdownToHtml content

renderPostLink :: String -> Post -> H.Html
renderPostLink rootPrefix Post{..} = do
  let title = maybe "" id $ lookup "title" tags
      date = maybe "" id $ lookup "date" tags
  H.li $ do
    H.em $ fromString date
    fromString " - "
    H.a ! A.href (fromString $ rootPrefix </> "posts/" </> postFilename filename) $ fromString title

renderIndex :: [Post] -> H.Html
renderIndex posts = defaultTemplate "functorial" "./" $ do
  H.h2 "Posts"
  H.ul $ mapM_ (renderPostLink ".") posts

writeHtml :: FilePath -> H.Html -> IO ()
writeHtml fp = writeFile fp . renderHtml

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
