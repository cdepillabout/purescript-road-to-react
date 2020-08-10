-- This is a PureScript implementation of
-- https://codesandbox.io/s/github/the-road-to-learn-react/hacker-stories/tree/hs/Inline-Handler-in-JSX?file=/src/App.js

module App (app) where

import Prelude

import Data.Array as Array
import Data.Newtype (class Newtype)
import Record (merge)
import Data.String as String
import Data.String.Pattern (Pattern(Pattern))
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect)
import Effect.Console (log)
import Prim.Row (class Lacks, class Union)
import React.Basic.DOM as R
import React.Basic.Hooks (Hook, JSX, ReactChildren, ReactComponent, UseEffect, UseState, coerceHook, component, element, fragment, reactChildrenFromArray, reactChildrenToArray, reactComponent, reactComponentWithChildren, useEffect, useState)
import React.Basic.Hooks as React

type Story = { title :: String, url :: String, author :: String, objectId :: Int }

initialStories :: Array Story
initialStories =
  [ { title: "React"
    , url: "https://reactjs.org/"
    , author: "Jordan Walke"
    , objectId: 0
    }
  , { title: "Redux"
    , url: "https://redux.js.org/"
    , author: "Dan Abramov, Andrew Clark"
    , objectId: 1
    }
  ]

newtype UseSemiPersistentState hooks
    = UseSemiPersistentState (UseEffect (String /\ String) (UseState String hooks))

derive instance newtypeUseSemiPersistentState :: Newtype (UseSemiPersistentState hooks) _

useSemiPersistentState
  :: String
  -> String
  -> Hook
      -- (UseEffect (String /\ String) (UseState String))
      UseSemiPersistentState
      (String /\ ((String -> String) -> Effect Unit))
useSemiPersistentState key initialState = coerceHook React.do
  value /\ setValue <- useState initialState

  useEffect (value /\ key) $ do
    log "useSemiPersistentState, useEffect, running again"
    pure mempty

  pure (value /\ setValue)


app :: Effect (ReactComponent {})
app = do
  inputWithLabel <- makeInputWithLabel

  reactComponent "App" \props -> React.do
    searchTerm /\ setSearchTerm <- useSemiPersistentState "search" "Re"
    stories /\ setStories <- useState initialStories

    let searchedStories =
          Array.filter
            (\story -> String.contains (Pattern searchTerm) story.title)
            stories

    -- pure (R.text (show searchedStories))
    pure $
      R.div_
        [ R.h1_ [ R.text "My Hacker Stories" ]
        , inputWithLabel
            ((inputWithLabelDef
              { children:
                  [ R.text "yo label yo" ]
              })
              { value = "this is not the default value yo"
              }
            )
        , R.hr {}
        , R.text "list"
        ]

-- source :: forall attrs attrs_. Union attrs attrs_ Props_source => Record attrs -> JSX

type PropsInputWithLabelReq = (children :: Array JSX)
type PropsInputWithLabel = (value :: String | PropsInputWithLabelReq)

inputWithLabelDef :: Record PropsInputWithLabelReq -> Record PropsInputWithLabel
inputWithLabelDef req =
  merge req
    { value: "YO THIS IS THE DEFAULT VALUE"
    }

makeInputWithLabel
  :: forall props
   . Effect (Record PropsInputWithLabel -> JSX)
makeInputWithLabel =
  component "InputWithLabel" \props -> React.do
    pure $
      fragment
        [ R.label
            { htmlFor: "lalala"
            , children: props.children
            }
        , R.text "&nbsp;"
        , R.input
            { value: props.value
            }
        ]

