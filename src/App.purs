-- This is a PureScript implementation of
-- https://codesandbox.io/s/github/the-road-to-learn-react/hacker-stories/tree/hs/Inline-Handler-in-JSX?file=/src/App.js

module App where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Record (merge, nub, union)
import Data.String as String
import Data.String.Pattern (Pattern(Pattern))
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect)
import Effect.Console (log)
import Prim.Row (class Lacks, class Nub, class Union)
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture, targetValue)
import React.Basic.Hooks (Hook, JSX, ReactChildren, ReactComponent, UseEffect, UseState, coerceHook, component, element, fragment, reactChildrenFromArray, reactChildrenToArray, reactComponent, reactComponentWithChildren, useEffect, useState)
import React.Basic.Hooks as React

class (Union inputProps optProps x, Nub x allProps) <=
  InputProps inputProps optProps allProps x

instance inputPropsC :: (Union inputProps optProps x, Nub x allProps) =>
  InputProps inputProps optProps allProps x

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
            { children:
                  [ R.text "yo label yo" ]
            , id: "search"
            , value: "this is not the default value yo"
            , onInputChange: \x -> log ("HELLLLLOOOOLLLLL 22222: " <> show x)
            }
        , R.hr {}
        , R.text "list"
        ]

type PropsInputWithLabel =
  ( id :: String
  , children :: Array JSX
  , onInputChange :: Maybe String -> Effect Unit
  | PropsInputWithLabelOpt
  )
type PropsInputWithLabelOpt = (value :: String)

makeInputWithLabel
  :: forall props x
   . InputProps props PropsInputWithLabelOpt PropsInputWithLabel x
  => Effect (Record props -> JSX)
makeInputWithLabel =
  component "InputWithLabel" \props_ -> React.do
    let def = { value: "YO THIS IS THE DEFAULT VALUE" }
        { id, children, onInputChange, value } = merge props_ def
    pure $
      fragment
        [ R.label
            { htmlFor: id
            , children: children
            }
        , R.text "&nbsp;"
        , R.input
            { id
            , value
            , onChange: capture targetValue onInputChange
            }
        ]

-- makeInputWithLabel
--   :: forall props
--   => Union props props_ PropsInputWithLabel
--    . Effect (Record props -> JSX)
-- makeInputWithLabel
--   :: forall props allProps allNubbedProps
--    . Union props PropsInputWithLabelOpt allProps
--   => Nub allProps allNubbedProps
--   => Effect (Record (PropsInputWithLabelReq optProps)) -> JSX)
-- makeInputWithLabel
--   :: forall optProps allProps allNubbedProps allOptProps allNubbedOptProps
--    . Union optProps PropsInputWithLabelOpt allOptProps
--   => Nub allOptProps allNubbedOptProps
--   => Union (PropsInputWithLabelReq optProps) PropsInputWithLabelOpt allProps
--   => Nub allProps allNubbedProps
--   => Effect (Record (PropsInputWithLabelReq optProps) -> JSX)
