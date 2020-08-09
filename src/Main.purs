module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import React.Basic.DOM (render)
import React.Basic.DOM as R
import React.Basic.Hooks (component, useState)
import React.Basic.Hooks as React
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

main :: Effect Unit
main = do
  -- _ <- component "App" \props -> React.do
  --   pure mempty
  -- pure unit

  log "main, starting..."
  win <- window
  doc <- document win
  -- root <- getElementById "root" =<< (map toNonElementParentNode $ document =<< window)
  root <- getElementById "app" (toNonElementParentNode doc)
  case root of
    Nothing -> do
      log "main, no app id element found..."
      throw "Root element not found."
    Just r  -> do
      log "main, found app id element...."
      render (R.text "balbhlabhal!!") r
      -- app <- mkApp
      -- render (element app {}) r
