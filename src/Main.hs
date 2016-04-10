{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Development.Shake
import Development.Shake.FilePath
import Data.Attoparsec.Text
import qualified Data.Text as T
import Data.Text (Text)
import Data.Function (on)
import Data.List (sortBy, nub)
import Data.Yaml hiding (Parser)
import Data.Char (toLower)
import Data.Maybe (isJust, fromMaybe)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Set as S
import Control.Monad
import Control.Applicative ((<|>))
import Text.Pandoc
import Text.Pandoc.Shared (stringify)
import Text.Pandoc.Readers.Markdown
import Text.Blaze.Renderer.Utf8
import System.Exit
import System.Process
import Lucid
import Lucid.Base

import Data.Conduit.Binary (sinkFile) -- Exported from the package conduit-extra
import Network.HTTP.Conduit
import qualified Data.Conduit as C
import Control.Monad.Trans.Resource (runResourceT)

--------------------------------------------------------------------------------
-- Pandoc
--------------------------------------------------------------------------------
myPandocExtensions :: S.Set Extension
myPandocExtensions =
  S.fromList [ Ext_link_attributes
             , Ext_mmd_link_attributes
             ]
--------------------------------------------------------------------------------
-- Page
--------------------------------------------------------------------------------
data Page = Page { pagePath     :: FilePath
                 , pageTemplate :: String
                 , pagePandoc   :: Pandoc
                 }

renderPage :: Page -> String
renderPage (Page _ template pandoc) =
  let optsPlain = def{ writerExtensions = S.union myPandocExtensions $
                                            writerExtensions def
                     , writerHighlight = True
                     , writerHtml5 = True
                     }
      optsTOC = optsPlain { writerTableOfContents = True
                          , writerTemplate = template --tocTmpl
                          , writerStandalone = True
                          }
      opts = optsTOC
      --tocTmpl = unwords ["<div class=\"col-md-3\" id=\"toc\">"
      --                  ,"<h2>Table of Contents</h2>"
      --                  ,"$toc$"
      --                  ,"</div>"
      --                  ,"<div class=\"col-md-9\">$body$</div>"
      --                  ]
  in writeHtmlString opts pandoc --return (const $ renderMarkupBuilder blazehtml, ())

--instance ToHtml Pandoc where
--  toHtml = pandocHtml
--  toHtmlRaw = toHtml
--------------------------------------------------------------------------------
-- Caches
--------------------------------------------------------------------------------
type GetMarkdown = FilePath -> Action Page
type GetTemplate = FilePath -> Action String

backupTemplate :: String
backupTemplate =
  "<html><head><title>$title$</title></head><body>$body$</body></html>"

makeGetTemplate :: Rules GetTemplate
makeGetTemplate = newCache $ \file -> do
  liftIO $ putStrLn $ "Looking for template: " ++ file
  doesFileExist file >>= \case
    False -> return backupTemplate
    True -> readFile' file

makeGetMarkdown :: GetTemplate -> Rules GetMarkdown
makeGetMarkdown getTemplate = newCache $ \file -> do
  let reader = readMarkdownWithWarnings myOpts
      myOpts = def{ readerExtensions = S.union myPandocExtensions $
                                         readerExtensions def
                  , readerSmart = True
                  }
  (pandoc@(Pandoc meta _),ws) <- (reader <$> readFile' file) >>= \case
    Left err -> liftIO $ print err >> exitFailure
    Right p  -> return p
  unless (null ws) $ liftIO $ putStrLn $ unlines $ "WARNINGS: ":ws

  let templateName = maybe "default" stringify $ lookupMeta "theme" meta
  liftIO $ putStrLn $ "Getting template: " ++ templateName
  template <- getTemplate $ "templates" </> templateName  <.> "html"

  let page = Page { pagePath = file
                  , pageTemplate = template
                  , pagePandoc = pandoc
                  }
  return page


milkShake :: Rules () -> IO ()
milkShake = shakeArgs shakeOptions{shakeFiles="build/"}

main :: IO ()
main = milkShake $ do
    getTemplate <- makeGetTemplate
    getMarkdown <- makeGetMarkdown getTemplate

    phony "clean" $ removeFilesAfter "build/" ["//*"]

    let needCSS = do css <- map ("build" </>) <$>
                                getDirectoryFiles "" ["css" </> "*.css"]
                     need css
        needImgs = do imgs <- map ("build" </>) <$>
                                  getDirectoryFiles "" ["img" </> "*.*"]
                      need imgs
        needRsrcs = needCSS >> needImgs
    ----------------------------------------------------------------------------
    -- build
    ----------------------------------------------------------------------------
    phony "build" $ do
        markdownPages <- getDirectoryFiles "content" ["//*.md"]
        let pages = map ((-<.> "html") . ("build" </>)) markdownPages
        need pages
        needRsrcs
    ----------------------------------------------------------------------------
    -- CSS
    ----------------------------------------------------------------------------
    "build" </> "css" </> "*.css" %> \out ->
        copyFile' (dropDirectory1 out) out
    ----------------------------------------------------------------------------
    -- images
    ----------------------------------------------------------------------------
    "build" </> "img" </> "*" %> \out ->
        copyFile' (dropDirectory1 out) out
    "build" <//> "*.html" %> \out -> do
      liftIO $ putStrLn $ "writing: " ++ out
      let md = "content" </> dropDirectory1 out -<.> "md"
      page <- getMarkdown md
      writeFile' out $ renderPage page
