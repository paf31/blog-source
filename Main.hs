{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Char
import Data.String
import Data.Monoid
import Data.Maybe
import Control.Arrow ((***), (&&&))
import Control.Applicative ((<$>))
import Data.List (intercalate, isSuffixOf, groupBy, nub, sort)
import Data.List.Split (splitOn, splitOneOf)
import System.IO (hPutStr, hClose)
import System.IO.Temp (withSystemTempFile)
import System.FilePath (dropExtension)
import System.Process (readProcess)
import System.Time
import System.Directory
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.String
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Feed.Util
import Text.Feed.Types
import qualified Text.RSS.Syntax as RSS
import Text.Feed.Export (xmlFeed)
import qualified Text.XML.Light.Output as XML

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
  posts <- filter (isSuffixOf ".markdown") <$> getDirectoryContents path
  flip mapM posts $ \filename -> do 
    content <- readFile $ path ++ "\\" ++ filename
    let split = readTags content
    return $ Post 
      { filename = filename
      , tags     = fst split
      , content  = intercalate "\n" $ snd split }
  
markdownToHtml :: String -> IO String
markdownToHtml = readProcess "pandoc" ["--from=markdown", "--to=html5", "--mathjax"]

postFilename :: String -> String
postFilename = (++ ".html") . dropExtension
	
toRssTime :: String -> String
toRssTime = toFeedDateString (RSSKind Nothing) . toClockTime . toCal . splitOneOf "/"
  where
    toCal [ yyyy, mm, dd ] = CalendarTime 
      { ctYear      = read yyyy
      , ctMonth     = toEnum (read mm - 1)
      , ctDay       = read dd
      , ctHour      = 0
      , ctMin       = 0
      , ctSec       = 0
      , ctPicosec   = 0
      , ctWDay      = Monday
      , ctYDay      = 0
      , ctTZName    = ""
      , ctTZ        = 0
      , ctIsDST     = False
	  }
    toCal ss = error $ "Invalid date format: " ++ intercalate "/" ss

collectTags :: [Post] -> [(String, [Post])]
collectTags posts = 
  map (id &&& postsFor posts) $ allTags posts
  where
  postsFor posts tag = filter (elem tag . tagsFor) posts
  tagsFor = map (dropWhile isSpace) . filter (not . null) . splitOneOf ",". fromJust . lookup "tags" . tags
  allTags = sort . nub . concatMap tagsFor
  
nbsp :: H.Html
nbsp = preEscapedToHtml ("&nbsp;" :: String)
  
mathJaxScript :: String
mathJaxScript = unlines
  [ "MathJax.Hub.Config({" 
  , "    config: ['MMLorHTML.js']," 
  , "    jax: ['input/TeX']," 
  , "    extensions: ['tex2jax.js']," 
  , "    TeX: {" 
  , "        extensions: ['AMSmath.js', 'AMSsymbols.js', 'noErrors.js','noUndefined.js']"
  , "    }," 
  , "    tex2jax: {" 
  , "        inlineMath: [['$','$'], ['\\\\(','\\\\)']]"
  , "    }"
  , "});" ]
  
gaq :: String
gaq = unlines
  [ "var _gaq = _gaq || [];" 
  , "_gaq.push(['_setAccount', 'UA-33896432-1']);" 
  , "_gaq.push(['_trackPageview']);" 
  , "(function() {" 
  , "  var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;" 
  , "  ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';" 
  , "  var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);" 
  , "})();" ]
  
disqus :: String
disqus = unlines
  [ "var disqus_shortname = 'functorial';"
  , "/* * * DON'T EDIT BELOW THIS LINE * * */"
  , "(function() {"
  , "var dsq = document.createElement('script'); dsq.type = 'text/javascript'; dsq.async = true;"
  , "    dsq.src = 'http://' + disqus_shortname + '.disqus.com/embed.js';"
  , "    (document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(dsq);"
  , "})();" ]
  
defaultTemplate :: String -> String -> Bool -> H.Html -> H.Html
defaultTemplate title rootPrefix useMathJax body = do
  H.docType 
  H.html $ do
    H.head $ do
      H.title $ H.toHtml $ "functorial.com - " ++ title
      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "http://fonts.googleapis.com/css?family=Lato:300,400,700"
      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href (fromString $ rootPrefix ++ "css/default.css")
      H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1.0"
      if useMathJax 
      then do
        H.script ! A.type_ "text/x-mathjax-config" $ preEscapedToHtml mathJaxScript  
        H.script ! A.type_ "text/javascript" ! A.src "http://cdn.mathjax.org/mathjax/latest/MathJax.js" $ mempty
      else mempty
      H.script ! A.type_ "text/javascript" $ preEscapedToHtml gaq
    H.body $ do
       H.div ! A.id "header" $ do
           H.div ! A.class_ "centered" $ do
               H.h1 $ H.a ! A.href (fromString $ rootPrefix ++ "index.html") ! A.style "text-decoration: none; color: white;" $ "functorial"
               H.p "Type Theory and Programming Languages Blog"
           H.div ! A.id "splitter" $ mempty
       H.div ! A.class_ "centered" $ do
           H.div ! A.id "navigation" $ do
               H.a ! A.href (fromString $ rootPrefix ++ "index.html") $ "Home"
               nbsp
               H.a ! A.href (fromString $ rootPrefix ++ "feed.xml") $ "RSS Feed"
           body

renderPost :: FilePath -> Post -> IO ()
renderPost dir post = do
  body <- markdownToHtml (content post)
  let 
    title = maybe "" id $ lookup "title" (tags post) 
    author = maybe "" id $ lookup "author" (tags post) 
    date = maybe "" id $ lookup "date" (tags post) 
    useMathJax = isJust $ lookup "math" (tags post) 
    html = renderHtml $ 
      defaultTemplate title "../" useMathJax $ do
        H.h2 $ fromString title
        H.p $ H.small $ fromString $ "by " ++ author ++ " on " ++ date
        H.hr
        preEscapedToHtml body
        H.hr
        H.div ! A.id "disqus_thread" $ do
          H.script ! A.type_ "text/javascript" $ preEscapedToHtml disqus
          H.noscript $ do
	  	  "Please enable JavaScript to view the "
	  	  H.a ! A.href "http://disqus.com/?ref_noscript" $ "comments powered by Disqus"
          H.a ! A.href "http://disqus.com" ! A.class_ "dsq-brlink" $ do
	  	  "Comments powered by "
	  	  H.span ! A.class_ "logo-disqus" $ "Disqus"
  writeFile (dir ++ (postFilename $ filename post)) html
  
renderPostLink :: String -> Post -> H.Html
renderPostLink rootPrefix post = 
  let
    title = maybe "" id $ lookup "title" (tags post) 
    date = maybe "" id $ lookup "date" (tags post) 
  in
    H.li $ do
      H.em $ fromString date
      fromString " - "
      H.a ! A.href (fromString $ rootPrefix ++ "posts/" ++ (postFilename $ filename post)) $ fromString title
  
renderTag :: FilePath -> (String, [Post]) -> IO ()
renderTag dir (name, posts) = do
  let 
    html = renderHtml $ 
      defaultTemplate "functorial" "../" False $ do		
        H.h2 $ fromString name
        H.ul $ mapM_ (renderPostLink "../") posts   
  writeFile (dir ++ name ++ ".html") html
	
renderIndex :: FilePath -> [String] -> [Post] -> IO ()
renderIndex dir ts posts = do
  let 
    html = renderHtml $ 
      defaultTemplate "functorial" "./" False $ do		
        H.h2 "Tags"
        H.ul $ flip mapM_ ts $ \tag -> 
          H.li ! A.style "float: left;" $ do
            H.a ! A.href (fromString $ "tags/" ++ tag ++ ".html") $ fromString tag
            nbsp
        H.div ! A.style "clear: left;" $ mempty
        H.h2 "All Posts"
        H.ul $ mapM_ (renderPostLink "./") posts 
  writeFile (dir ++ "index.html") html

renderFeed :: FilePath -> [Post] -> IO ()
renderFeed dir posts = do
  time <- getClockTime
  let 
    rss = (RSS.nullRSS "functorial" "http://blog.functorial.com")
	{ RSS.rssChannel = (RSS.nullChannel "functorial" "http://blog.functorial.com")
	  { RSS.rssItems = flip map posts $ \post ->
        (RSS.nullItem (maybe "" id $ lookup "title" $ tags post)) 
        { RSS.rssItemLink = Just $ "http://blog.functorial.com/posts/" ++ (postFilename (filename post))
        , RSS.rssItemDescription = lookup "description" $ tags post
		, RSS.rssItemPubDate = lookup "date" (tags post) >>= return . toRssTime
        , RSS.rssItemGuid = Just $ RSS.nullPermaGuid $ "http://blog.functorial.com/posts/" ++ (postFilename (filename post))
        }
      , RSS.rssLastUpdate = Just $ toFeedDateString (RSSKind Nothing) time
      , RSS.rssPubDate = Just $ toFeedDateString (RSSKind Nothing) time
      }
    }
  writeFile (dir ++ "feed.xml") $ XML.ppElement $ xmlFeed (RSSFeed rss)
  
main :: IO ()
main = do 
  dir <- getCurrentDirectory
  let 
    dirOutput = dir ++ "\\output\\"
    dirPosts  = dir ++ "\\output\\posts\\"
    dirCss    = dir ++ "\\output\\css\\"
    dirTags   = dir ++ "\\output\\tags\\"
  mapM_ (createDirectoryIfMissing False) [ dirOutput, dirCss, dirPosts, dirTags ]
  copyFile (dir ++ "\\css\\default.css") (dirCss ++ "default.css")
  posts <- getAllPosts $ dir ++ "\\posts"
  mapM_ (renderPost dirPosts) posts
  let tags = collectTags posts
  mapM_ (renderTag dirTags) tags
  renderIndex dirOutput (map fst tags) posts
  renderFeed dirOutput posts
  return ()