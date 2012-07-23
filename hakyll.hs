{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (id)
import Control.Category (id)
import Control.Arrow ((>>>), (***), arr)
import Data.Monoid (mempty, mconcat)
import Text.Pandoc.Shared (HTMLMathMethod(..), WriterOptions(..), defaultWriterOptions)
import Text.Pandoc.Parsing (defaultParserState)

import Hakyll

writerOptions :: WriterOptions
writerOptions = defaultWriterOptions 
  { writerHTMLMathMethod = MathJax "" 
  }

feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
  { feedTitle = "blog.functorial.com"
  , feedDescription = ""
  , feedAuthorName = "Phil Freeman"
  , feedAuthorEmail = "paf31@cantab.net"
  , feedRoot = "http://blog.functorial.com" }

main :: IO ()
main = hakyll $ do
    -- Compress CSS
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    -- Render posts
    match "posts/*" $ do
        route   $ setExtension ".html"
        compile $ pageCompilerWith defaultParserState writerOptions
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

    -- Index
    match "index.html" $ route idRoute
    create "index.html" $ constA mempty
        >>> arr (setField "title" "blog.functorial.com")
        >>> requireA "tags" (setFieldA "tagcloud" (renderTagCloud'))
        >>> requireAllA "posts/*" addPostList
        >>> applyTemplateCompiler "templates/index.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

    -- Tags
    create "tags" $
        requireAll "posts/*" (\_ ps -> readTags ps :: Tags String)

    -- Add a tag list compiler for every tag
    match "tags/*" $ route $ setExtension ".html"
    metaCompile $ require_ "tags"
        >>> arr tagsMap
        >>> arr (map (\(t, p) -> (tagIdentifier t, makeTagList t p)))

    -- RSS Feed
    match "feed.xml" $ route idRoute
    create "feed.xml" $ requireAll_ "posts/*" 
        >>> arr (take 10 . reverse . chronological)
        >>> renderRss feedConfig

    -- Read templates
    match "templates/*" $ compile templateCompiler
  where
    renderTagCloud' :: Compiler (Tags String) String
    renderTagCloud' = renderTagCloud tagIdentifier 100 120

    tagIdentifier :: String -> Identifier (Page String)
    tagIdentifier = fromCapture "tags/*"

addPostList :: Compiler (Page String, [Page String]) (Page String)
addPostList = setFieldA "posts" $
    arr (reverse . chronological)
        >>> require "templates/postitem.html" (\p t -> map (applyTemplate t) p)
        >>> arr mconcat
        >>> arr pageBody

makeTagList :: String
            -> [Page String]
            -> Compiler () (Page String)
makeTagList tag posts =
    constA (mempty, posts)
        >>> addPostList
        >>> arr (setField "title" ("Posts tagged '" ++ tag ++ "'"))
        >>> applyTemplateCompiler "templates/tag.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler
