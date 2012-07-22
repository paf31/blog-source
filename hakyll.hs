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
        >>> requireAllA "posts/*" addPostList
        >>> applyTemplateCompiler "templates/index.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

    -- RSS Feed
    match "feed.xml" $ route idRoute
    create "feed.xml" $ requireAll_ "posts/*" 
        >>> arr (take 10 . reverse . chronological)
        >>> renderRss feedConfig

    -- Read templates
    match "templates/*" $ compile templateCompiler

addPostList :: Compiler (Page String, [Page String]) (Page String)
addPostList = setFieldA "posts" $
    arr (reverse . chronological)
        >>> require "templates/postitem.html" (\p t -> map (applyTemplate t) p)
        >>> arr mconcat
        >>> arr pageBody
