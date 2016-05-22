{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Development.Shake
import Development.Shake.FilePath
import qualified Data.Text as T
import Data.Text (Text)
import Data.Yaml hiding (Parser)
import Data.Monoid ((<>))
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Lazy as M
import qualified Data.Set as S
import Control.Monad
import Text.Pandoc
import Text.Pandoc.Shared
import Text.Pandoc.XML
import Text.Pandoc.Readers.Markdown
import System.Exit
--------------------------------------------------------------------------------
-- Pandoc
--------------------------------------------------------------------------------
myPandocExtensions :: S.Set Extension
myPandocExtensions =
  S.fromList [ Ext_link_attributes
             , Ext_mmd_link_attributes
             ]

stringifyHTML :: MetaValue -> Value
stringifyHTML = String . T.pack . escapeStringForXML . stringify
--------------------------------------------------------------------------------
-- Page
--------------------------------------------------------------------------------
data Page = Page { pagePath     :: FilePath
                 , pageTemplate :: Text
                 , pagePandoc   :: Pandoc
                 }

renderTemplateTextWith :: Text -> Value -> String
renderTemplateTextWith template val =
  either (const $ T.unpack backupTemplate)
         (`renderTemplate` val)
         $ compileTemplate template

renderPage :: Page -> Object -> String
renderPage (Page _ template pandoc@(Pandoc meta _)) vars =
  let optsPlain  = def{ writerExtensions = S.union myPandocExtensions $
                                             writerExtensions def
                      , writerHighlight = True
                      , writerHtml5 = True
                      }
      opts = optsPlain{ writerStandalone = False
                      , writerTableOfContents = True
                      }
      body = String $ T.pack $ writeHtmlString opts pandoc
      toc = String $ T.pack $ writeHtmlString opts{writerTemplate = "$toc$"
                                                  ,writerStandalone = True
                                                  } pandoc
      bodyTOCVars = HM.fromList [("body", body), ("toc", toc)]
      metaVars = M.foldlWithKey (\acc k v -> HM.insert (T.pack k) (stringifyHTML v) acc) HM.empty $ unMeta meta
      allVars = vars <> metaVars <> bodyTOCVars
      obj = Object allVars
  in renderTemplateTextWith template obj
--------------------------------------------------------------------------------
-- Caches
--------------------------------------------------------------------------------
type GetMarkdown = FilePath -> Action Page
type GetTemplate = FilePath -> Action Text

backupTemplate :: Text
backupTemplate =
  "<html><head><title>$title$</title></head><body>$body$</body></html>"

makeGetTemplate :: Rules GetTemplate
makeGetTemplate = newCache $ \file -> do
  liftIO $ putStrLn $ "Looking for template: " ++ file
  doesFileExist file >>= \case
    False -> return backupTemplate
    True -> T.pack <$> readFile' file

makeGetMarkdown :: GetTemplate -> Rules GetMarkdown
makeGetMarkdown getTemplate = newCache $ \file -> do
  let reader = readMarkdownWithWarnings myOpts
      myOpts = def{ readerExtensions = S.union myPandocExtensions $
                                         readerExtensions def
                  , readerSmart = True
                  }
  (pandoc@(Pandoc meta _),ws) <- (reader <$> readFile' file) >>= \case
    Left er -> liftIO $ print er >> exitFailure
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
      writeFile' out $ renderPage page HM.empty
