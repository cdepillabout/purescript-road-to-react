
module App (app) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import React.Basic.DOM (render)
import React.Basic.DOM as R
import React.Basic.Hooks (ReactComponent, reactComponent, useState)
import React.Basic.Hooks as React
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

app :: Effect (ReactComponent {})
app =
  reactComponent "App" \props -> React.do
    pure (R.text "yoyoyoyyyyy")
  --   pure mempty
  -- pure unit
